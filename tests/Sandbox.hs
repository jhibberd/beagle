import Beagle.Domain
import Beagle.Eval2
import Debug.Trace

main = print $ eval gt 0 (0, traceShow start start)
    where start = [1, 3, 5, 2, 6, 4]
          gt = [Empty,Empty,RotateR,SwapR,Empty,MoveL,SwapL,MoveL,SwapL,SwapL,MoveR,MoveR,MoveR,Empty,SwapR,Empty,Empty,RotateR,MoveL,SwapR]

