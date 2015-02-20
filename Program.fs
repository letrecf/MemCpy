open System.Reflection.Emit
open System.Runtime.InteropServices
open System.Security.Permissions
open System.Threading

type blitPtrDelegate = delegate of nativeint * nativeint * uint32 -> unit
type blitDelegate<'a when 'a : unmanaged> = delegate of 'a array * 'a array * uint32 -> unit
type blitFromDelegate<'a when 'a : unmanaged> = delegate of nativeint * 'a array * uint32 -> unit
type blitToDelegate<'a when 'a : unmanaged> = delegate of 'a array * nativeint * uint32 -> unit

let emitAddressOfFirstElement<'a when 'a:unmanaged> (ilGenerator : ILGenerator) = 
  ilGenerator.Emit(OpCodes.Ldc_I4_0)
  ilGenerator.Emit(OpCodes.Ldelema,typeof<'a>)

let emitMultiplyBySize<'a when 'a:unmanaged> (ilGenerator : ILGenerator) = 
  ilGenerator.Emit(OpCodes.Sizeof,typeof<'a>)
  ilGenerator.Emit(OpCodes.Mul)

[<SecurityPermission(SecurityAction.Demand, UnmanagedCode=true)>]
type public MemCpyPtr () =
  static let createBlitDelegate =
    let dm = new DynamicMethod("blitPtr",
                                typeof<System.Void>,
                                [| typeof<nativeint>; typeof<nativeint>; typeof<System.UInt32> |],
                                typeof<string>.Module
                                )
    let ilGenerator = dm.GetILGenerator()
    ilGenerator.Emit(OpCodes.Ldarg_0)
    ilGenerator.Emit(OpCodes.Ldarg_1)
    ilGenerator.Emit(OpCodes.Ldarg_2)
    ilGenerator.Emit(OpCodes.Unaligned,1uy) // always align to byte boundary since the data size is unknown
    ilGenerator.Emit(OpCodes.Cpblk)
    ilGenerator.Emit(OpCodes.Ret)
    dm.CreateDelegate(typeof<blitPtrDelegate>) :?> blitPtrDelegate
  static member Blit src dst length = createBlitDelegate.Invoke(dst, src, length)

[<SecurityPermission(SecurityAction.Demand, UnmanagedCode=true)>]
type public MemCpy<'a when 'a : unmanaged> () =
  static let unaligned = sizeof<'a> < sizeof<nativeint>
  static let alignment = uint8 sizeof<'a>
  static let createBlitDelegate =
    let dm = new DynamicMethod("blit",
                                typeof<System.Void>,
                                [| typeof<'a array>; typeof<'a array>; typeof<System.UInt32> |],
                                typeof<string>.Module
                                )
    let ilGenerator = dm.GetILGenerator()
    ilGenerator.Emit(OpCodes.Ldarg_0)
    emitAddressOfFirstElement<'a> ilGenerator
    ilGenerator.Emit(OpCodes.Ldarg_1)
    emitAddressOfFirstElement<'a> ilGenerator
    ilGenerator.Emit(OpCodes.Ldarg_2)
    emitMultiplyBySize<'a> ilGenerator
    if unaligned then ilGenerator.Emit(OpCodes.Unaligned,alignment) // align 
    ilGenerator.Emit(OpCodes.Cpblk)
    ilGenerator.Emit(OpCodes.Ret)
    dm.CreateDelegate(typeof<blitDelegate<'a>>) :?> blitDelegate<'a>
  static let createBlitFromDelegate =
    let dm = new DynamicMethod("blitFrom",
                                typeof<System.Void>,
                                [| typeof<nativeint>; typeof<'a array>; typeof<System.UInt32> |],
                                typeof<string>.Module
                                )
    let ilGenerator = dm.GetILGenerator()
    ilGenerator.Emit(OpCodes.Ldarg_0)
    ilGenerator.Emit(OpCodes.Ldarg_1)
    emitAddressOfFirstElement<'a> ilGenerator
    ilGenerator.Emit(OpCodes.Ldarg_2)
    emitMultiplyBySize<'a> ilGenerator
    if unaligned then ilGenerator.Emit(OpCodes.Unaligned,alignment) // align 
    ilGenerator.Emit(OpCodes.Cpblk)
    ilGenerator.Emit(OpCodes.Ret)
    dm.CreateDelegate(typeof<blitFromDelegate<'a>>) :?> blitFromDelegate<'a>
  static let createBlitToDelegate =
    let dm = new DynamicMethod("blitTo",
                                typeof<System.Void>,
                                [| typeof<'a array>; typeof<nativeint>; typeof<System.UInt32> |],
                                typeof<string>.Module
                                )
    let ilGenerator = dm.GetILGenerator()
    ilGenerator.Emit(OpCodes.Ldarg_0)
    emitAddressOfFirstElement<'a> ilGenerator
    ilGenerator.Emit(OpCodes.Ldarg_1)
    ilGenerator.Emit(OpCodes.Ldarg_2)
    emitMultiplyBySize<'a> ilGenerator
    if unaligned then ilGenerator.Emit(OpCodes.Unaligned,alignment) // align 
    ilGenerator.Emit(OpCodes.Cpblk)
    ilGenerator.Emit(OpCodes.Ret)
    dm.CreateDelegate(typeof<blitToDelegate<'a>>) :?> blitToDelegate<'a>
  static member Blit src dst = createBlitDelegate.Invoke(dst, src, uint32 src.Length)
  static member BlitFrom src dst = createBlitFromDelegate.Invoke(dst, src, uint32 src.Length)
  static member BlitTo src dst = createBlitToDelegate.Invoke(dst, src, uint32 dst.Length)

type HeapArray<'a when 'a : unmanaged> (s:int) =
  [<VolatileField>]
  let mutable disposed = 0
  let nativeArr = Marshal.AllocHGlobal(s * sizeof<'a>)

  new (arg : 'a array) as this =
    new HeapArray<'a>(arg.Length) then MemCpy<'a>.BlitFrom arg this.Ptr

  member x.Ptr = nativeArr

  member x.Data =
    let ret : 'a array = Array.zeroCreate s
    MemCpy<'a>.BlitTo x.Ptr ret
    ret
    
  interface System.IDisposable with
    member x.Dispose () =
      if Interlocked.CompareExchange(&disposed, 1, 0) = 0 then
        Marshal.FreeHGlobal nativeArr


[<EntryPoint>]
let main argv = 
  // copy array to array
  let a = [| 1.0; 2.0; 4.0 |]
  let b : float array = Array.zeroCreate a.Length
  MemCpy<float>.Blit a b
  printfn "b:%A" b

  // init c by using a 
  let c = new HeapArray<float>(a)
  printfn "c: %A" c.Data
  // overwrite first two elements in c
  let d = [| 8.0; 9.0 |]
  MemCpy<float>.BlitFrom d c.Ptr
  let cd = c.Data
  printfn "c: %A" cd

  // MemCpyPtr
  let u = new HeapArray<float>(cd.Length)
  MemCpyPtr.Blit c.Ptr u.Ptr (uint32 (cd.Length*sizeof<float>))
  printfn "u: %A" u.Data

  // byte arrays
  let h = [| 9uy; 8uy; 7uy; 6uy|]
  let g : byte array = Array.zeroCreate a.Length
  MemCpy<byte>.Blit h g
  printfn "g:%A" g

  0 // return an integer exit code
