module AOC18
    ( Acre(..)
    , Area(..)
    , Content(..)
    , adjacentAcres
    , adjacentAcresOf
    , fromSchematic
    , multiTransformArea
    , scanFile
    , toSchematic
    , totalResourceValue
    , transformArea
    )
    where  

import Data.Array (( ! ), Array, assocs, bounds , elems, listArray)
import Data.List (foldl', iterate)


type Position = (Int, Int)
type Error = String

data Content = Open | Trees | Lumberyard deriving (Eq, Show)

data Acre = Acre {_pos :: Position, _content :: Content} deriving (Eq, Show)

newtype Area = Area {_area :: (Array (Int, Int) Acre)} deriving (Eq, Show) -- one-based


newlineChar :: Char
newlineChar = '\n'


iconToContent :: Char -> Either Error Content
iconToContent char =
    case char of
        '.' -> Right Open
        '|' -> Right Trees
        '#' -> Right Lumberyard
        _   -> Left $ "Invalid icon: " ++ [char] 


contentToIcon :: Content -> Char
contentToIcon x =
    case x of
        Open -> '.'
        Trees -> '|'
        Lumberyard -> '#'


scanFile :: Int -> Int -> FilePath -> IO (Either Error Area)
scanFile width height filePath = do
    contents <- readFile filePath
    pure $ fromSchematic width height contents


fromSchematic :: Int -> Int -> String -> Either Error Area
fromSchematic width height string =
    let
        eiResult = 
            foldl'
                ( \ acc char -> 
                    case acc of
                        Right ( currentPos@(y, x), as ) ->
                            if char == newlineChar then
                                if x < width + 1 then
                                    Left $ "Line too short: Width of line " ++ show y ++ " must be " ++ show width
                                else
                                    Right $ ( (y+1, 1), as )
                            else if x == width + 1 then
                                Left $ "Line too long: Width of line " ++ show y ++ " must be " ++ show width
                            else
                                if y > height then
                                    Left $ "Too many lines: Number of lines must be " ++ show height
                                else
                                    let
                                        eiContent = iconToContent char
                                    in 
                                        case eiContent of
                                            Right content ->
                                                Right $ ( (y, x+1), as ++ [Acre currentPos content] )
                                            Left s ->
                                                Left $ s ++ " (at " ++ show currentPos ++ ")"

                        Left s -> -- only report first error
                            Left s                
                )
                (Right ( (1,1), [] ))
                string
    in
        case eiResult of 
            Right (_, as) -> 
                if length as < width * height then -- at this point, all widths ok; so height must be wrong
                    Left $ "Too few lines: Number of lines must be " ++ show height
                else
                    Right $ Area $ listArray ((1,1), (width,height)) as
            
            Left s -> 
                Left s


toSchematic :: Area -> String
toSchematic (Area area) = 
    let
        ((_,_), (width,_)) = bounds area
    in
        concatMap
            ( \ ((y,x), (Acre _ content)) -> 
                let
                    char = contentToIcon content 
                    isEndOfLine = x == width
                in
                    if isEndOfLine then [char] ++ [newlineChar] else [char]        
            )
            (assocs area)


adjacentPositions :: Area -> Position -> [Position]
adjacentPositions area (y,x) =        
    let
        ((minY, minX), (maxY, maxX)) = bounds $ _area area

        candidates = 
            [ (y-1, x-1), (y-1, x), (y-1, x+1)
            , (y,   x-1),           (y,   x+1)
            , (y+1, x-1), (y+1, x), (y+1, x+1)
            ]
    in
        filter
            (\ (y', x') -> (x' >= minX && x' <= maxX) && (y' >= minY && y' <= maxY))
            candidates


adjacentAcres :: Area -> Acre -> [Acre]
adjacentAcres area acre =
    map
        (\ pos -> _area area ! pos)
        (adjacentPositions area $ _pos acre)


adjacentAcresOf :: Area -> Acre -> Content -> [Acre]
adjacentAcresOf area acre content =
    filter
        (\ a -> _content a == content)
        (adjacentAcres area acre)


acresOf :: [Acre] -> Content -> [Acre]    
acresOf acres content =
    filter
        (\ a -> _content a == content)
        acres
    

acres :: Area -> [Acre]
acres area =
    elems $ _area area


transformContentTo :: Acre -> Content -> Acre 
transformContentTo acre content =
    Acre (_pos acre) content


transformAcre :: Area -> Acre -> Acre 
transformAcre area acre =
    case _content acre of             
        Open -> 
            if length  (adjacentAcresOf area acre Trees) >= 3 then
                transformContentTo acre Trees
            else
                acre

        Trees -> 
            if length (adjacentAcresOf area acre Lumberyard) >= 3 then
                transformContentTo acre Lumberyard
            else
                acre

        Lumberyard ->    
            if (length (adjacentAcresOf area acre Lumberyard) >= 1) &&
               (length (adjacentAcresOf area acre Trees) >= 1) then                
                acre
            else
                transformContentTo acre Open

                
transformArea :: Area -> Area
transformArea area = 
    let
        transformedAcres = 
            map
                (\ acre -> transformAcre area acre)
                (acres area)
    in
        Area $ listArray (bounds $ _area area) transformedAcres


multiTransformArea :: Area -> Int -> Area
multiTransformArea area n = 
    -- first element in the list is the input
    last $ take (n+1) $ iterate transformArea area


totalResourceValue :: Area -> Int
totalResourceValue area =   
    let
        theAcres = acres area        
    in
        (length $ acresOf theAcres Trees)
        *
        (length $ acresOf theAcres Lumberyard)
    