module Main where

import Control.Monad
import Control.Applicative
import System.IO
import System.Process
import System.Environment
import Data.Foldable

import qualified Data.Set as S 

type FileName = String
type Point = (Int,Int)
type PointSet = S.Set Point 

readpt s = let (x,y) = span (/= ',') s
           in (read x, read (tail y))
              



getboxes :: FileName -> IO [Point]
getboxes f = do pts <- lines <$> readFile f
                return $ map readpt pts
                
neighbors :: Point -> [Point]
neighbors (x,y) =  [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

floodstep :: [Point] -> PointSet -> PointSet -> (PointSet,PointSet)
floodstep [] pts seen = (seen, (S.difference pts seen))
floodstep (t:ts) pts seen = let n = filter (\p -> (S.member p pts) && (not $ S.member p seen)) (neighbors t)
                            in floodstep (n ++ ts) pts (S.union seen (S.fromList n))

flood pts | S.null pts = []
flood pts = let h = (head . S.toList) pts
                (some, rest) = floodstep [h] pts (S.singleton h)
            in (some:flood rest)

test = do b <- getboxes "out.txt"
          return $ flood (S.fromList b)
          
bounds s = let xs = S.map fst s
               ys = S.map snd s
           in ((S.findMin xs, S.findMin ys), (S.findMax xs, S.findMax ys))

----
geometry s padding scale = let ((x,y),(z,w)) = bounds s 
                               a = scale * (x+padding)
                               b = scale * (y+padding)
                               dx = scale * (z-x-2*padding)
                               dy = scale * (w-y-2*padding)
                           in (show dx)++"x"++(show dy)++"+"++(show a)++"+"++(show b)
                        

{-
main = do pts <- lines <$> getContents
          let boxes = map readpt pts
          mapM_ (putStrLn . (\b -> geometry b 5 4)) (flood (S.fromList boxes))
-}
                      
main = compute

commands :: FileName -> [(String,[String])]  
commands fn = [("convert",[fn, "text:-"]),
               ("grep", ["magenta"]),
               ("awk", ["'{ print substr($1,1,length($1)-1) }'"])]


callwith hdl (c,args) 
  = do (_,Just h,_,_) <- createProcess (proc c args){std_in=UseHandle hdl, std_out=CreatePipe}
       return h



cropfile fn out g  = do (_,_,_,p) <- createProcess (proc "convert" ["-density",
                                                                    "288",
                                                                    fn,
                                                                    "-crop",
                                                                    g,
                                                                    out])
                        return p


compute = do filename <- head <$> getArgs
             let comms = [("convert",[filename,"text:-"]),
                          ("grep",["magenta"]),
                          ("awk",["{ print substr($1,1,length($1)-1)}"])]           
             h <- openFile filename ReadMode
             pixels <- foldlM callwith h comms 
             pts <- lines <$> hGetContents pixels
             let boxes = map readpt pts
             let crops = map (\b -> geometry b 5 4) (flood (S.fromList boxes))
             zipWithM_ (\n g -> cropfile filename ("fig"++(show n)++".png") g) [0 ..] crops


--- possible workflow: sample at 72, compute box, export at 288

