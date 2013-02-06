module Main where

import Control.Monad
import Control.Applicative
import System.IO
import System.Process
import System.Environment
import Data.Foldable (foldlM)

import qualified Data.Set as S 

type FileName = String
type Point = (Int,Int)
type PointSet = S.Set Point 

readpt :: String -- | (x,y)  
          -> Point 
readpt s = let (x,y) = span (/= ',') s
           in (read x, read (tail y))
              
-- | List of all four neighbors in cardinal directions                
neighbors :: Point -> [Point]
neighbors (x,y) =  [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

-- | Helper function to flood
floodstep :: [Point] -> -- | unrelaxed vertices 
             PointSet -> -- | set of vertices not in a labelled component 
             PointSet -> -- | set of vertices ready to be added to new component 
             (PointSet,PointSet) -- | (new component, remaining vertices)
floodstep [] pts seen = (seen, (S.difference pts seen))
floodstep (t:ts) pts seen = let n = filter (\p -> (S.member p pts) && (not $ S.member p seen)) (neighbors t)
                            in floodstep (n ++ ts) pts (S.union seen (S.fromList n))

-- | Computes list of connected components using flood fill algorithm
flood :: PointSet -> [PointSet]
flood pts | S.null pts = []
flood pts = let h = (head . S.toList) pts
                (some, rest) = floodstep [h] pts (S.singleton h)
            in (some:flood rest)
          
-- | Computes bounding box for a PointSet               
bounds :: PointSet -> 
          (Point,Point) -- | (upper left corner, lower right corner)
bounds s = let xs = S.map fst s
               ys = S.map snd s
           in ((S.findMin xs, S.findMin ys), (S.findMax xs, S.findMax ys))
              
-- | Produces geometry string for imagemagick "widthxheight+xoff+yoff"
geometry :: PointSet -- | Set of points 
            -> Int -- | Internal padding 
            -> Int -- | Upscaling factor (ex. 4 for 288 dpi)
            -> String  
geometry s padding scale = let ((x,y),(z,w)) = bounds s 
                               a = scale * (x+padding)
                               b = scale * (y+padding)
                               dx = scale * (z-x-2*padding)
                               dy = scale * (w-y-2*padding)
                           in (show dx)++"x"++(show dy)++"+"++(show a)++"+"++(show b)
                                              

commands :: FileName -> [(String,[String])]  
commands fn = [("convert",[fn, "text:-"]),
               ("grep", ["magenta"]),
               ("awk", ["'{ print substr($1,1,length($1)-1) }'"])]

callwith :: Handle -- | input handle
            -> (String, [String]) -- | Process & args
            -> IO Handle -- | output handle
callwith hdl (c,args) 
  = do (_,Just h,_,_) <- createProcess (proc c args){std_in=UseHandle hdl, std_out=CreatePipe}
       return h

cropfile :: FileName -- | input file
            -> FileName -- | output file
            -> String -- | geometry for cropping
            -> IO ProcessHandle 
cropfile fn out g  = do (_,_,_,p) <- createProcess (proc "convert" ["-density",
                                                                    "288",
                                                                    fn,
                                                                    "-crop",
                                                                    g,
                                                                    out])
                        return p


main = compute


compute = do [filename,prefix] <- getArgs
             let comms = [("convert",[filename,"text:-"]),
                          ("grep",["magenta"]),
                          ("awk",["{ print substr($1,1,length($1)-1)}"])]           
             h <- openFile filename ReadMode
             pixels <- foldlM callwith h comms 
             pts <- lines <$> hGetContents pixels
             let boxes = map readpt pts
             let crops = map (\b -> geometry b 5 4) (flood (S.fromList boxes))
             procs <- zipWithM (\n g -> cropfile filename (prefix++(show n)++".png") g) [0 ..] crops
             mapM_ waitForProcess procs
             

