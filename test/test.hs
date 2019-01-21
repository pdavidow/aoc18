import Test.Tasty
import Test.Tasty.HUnit

import Data.Either (fromLeft, fromRight)
import AOC18 (Acre(..), Area(..), Content(..), adjacentAcres, adjacentAcresOf, fromSchematic, multiTransformArea, scanFile, toSchematic, totalResourceValue, transformArea) 
import Data.Array (listArray, (!))

main = defaultMain tests
  
tests :: TestTree
tests = testGroup "Tests" [unitTests] 

defaultArea :: Area
defaultArea =
    Area $ listArray ((1,1),(1,1)) [Acre (1,1) Open]


unitTests = testGroup "Unit tests" $

    [ testGroup "AOC18" $ 
        [ testGroup "1" $   
            let
                area = fromRight defaultArea $ 
                    fromSchematic 
                        10 
                        10
                        ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|." 
            in
                [ testCase "totalResourceValue" $  
                     totalResourceValue (multiTransformArea area 10) @?= 1147

                , testCase "toSchematic" $ 
                    toSchematic area @?= ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.\n"                                    
                
                , testCase "toSchematic after multiTransformArea 1" $ 
                     toSchematic (multiTransformArea area 1) @?= ".......##.\n......|###\n.|..|...#.\n..|#||...#\n..##||.|#|\n...#||||..\n||...|||..\n|||||.||.|\n||||||||||\n....||..|.\n"
                
                , testCase "toSchematic after multiTransformArea 2" $ 
                     toSchematic (multiTransformArea area 2) @?= ".......#..\n......|#..\n.|.|||....\n..##|||..#\n..###|||#|\n...#|||||.\n|||||||||.\n||||||||||\n||||||||||\n.|||||||||\n"               
                ]

        , testGroup "2" $   
            let
                error1 = fromLeft "uhoh" $ 
                    fromSchematic 
                        10 
                        9
                        ".#.#.*.|#.\n.....#|##|\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."   
                        
                error2 = fromLeft "uhoh" $ 
                    fromSchematic 
                        10 
                        9
                        ".#.#...|#.\n....#|##|\n.|..|...#.\n..|#.....#\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."   
                         
                error3 = fromLeft "uhoh" $ 
                    fromSchematic 
                        10 
                        9
                        ".#.#...|#.\n.....#|##|\n.|..|....#.\n..|#.....#\n#.#|||#|#|\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."   
                            
                error4 = fromLeft "uhoh" $ 
                    fromSchematic 
                        10 
                        9
                        ".#.#...|#.\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n||...#|.#|\n|.||||..|.\n...#.|..|."                           

                error5 = fromLeft "uhoh" $ 
                    fromSchematic 
                        10 
                        9
                        ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.\n...#.|..|."                                                   
            in
                [ testCase "Invalid icon error" $  
                    error1 @?= "Invalid icon: * (at (1,6))"     

                , testCase "Invalid width, too short" $  
                    error2 @?= "Line too short: Width of line 2 must be 10"   

                , testCase "Invalid width, too long" $  
                    error3 @?= "Line too long: Width of line 3 must be 10"   
                    
                , testCase "Invalid height, too short" $  
                    error4 @?= "Too few lines: Number of lines must be 9"  

                , testCase "Invalid height, too long" $  
                    error5 @?= "Too many lines: Number of lines must be 9"                       
                ]

        , testGroup "3" $   
            let
                area = fromRight defaultArea $ 
                    fromSchematic 
                        3 
                        3
                        "|||\n|.|\n|||"

                (Area array) = area     
            in
                [ testCase "toSchematic" $ 
                    toSchematic area @?= "|||\n|.|\n|||\n"                                    
                    
                , testCase "toSchematic after transformArea" $ 
                    toSchematic (transformArea area) @?= "|||\n|||\n|||\n"

                , testCase "toSchematic after multiTransformArea 2" $ 
                    toSchematic (multiTransformArea area 2) @?= "|||\n|||\n|||\n"  

                , testCase "adj" $
                    length (adjacentAcresOf area (array ! (2,2)) Trees)  @?= 8                   
                ]

        , testGroup "4" $   
            let
                area = fromRight defaultArea $ 
                    fromSchematic 
                        3 
                        3
                        "###\n#|#\n###"
 
                (Area array) = area     
            in
                [ testCase "toSchematic" $ 
                    toSchematic area @?= "###\n#|#\n###\n"                                    
                    
                , testCase "toSchematic after transformArea" $ 
                    toSchematic (transformArea area) @?= "###\n###\n###\n"

                , testCase "toSchematic after multiTransformArea 2" $ 
                    toSchematic (multiTransformArea area 2) @?= "...\n...\n...\n"                    

                , testCase "adj" $
                    length (adjacentAcresOf area (array ! (2,2)) Lumberyard)  @?= 8                    
                ]

        , testGroup "5" $   
            let
                area = fromRight defaultArea $ 
                    fromSchematic 
                        3 
                        3
                        "...\n.#.\n..."
 
                (Area array) = area     
            in
                [ testCase "toSchematic" $ 
                    toSchematic area @?= "...\n.#.\n...\n"                                    
                    
                , testCase "toSchematic after transformArea" $ 
                    toSchematic (transformArea area) @?= "...\n...\n...\n"

                , testCase "toSchematic after multiTransformArea 2" $ 
                    toSchematic (multiTransformArea area 2) @?= "...\n...\n...\n"                       

                , testCase "adj" $
                    length (adjacentAcresOf area (array ! (2,2)) Open)  @?= 8                    
                ]

        , testGroup "6" $   
            let
                area = fromRight defaultArea $ 
                    fromSchematic 
                        3 
                        3
                        "###\n###\n###"
  
                (Area array) = area     
            in
                [ testCase "toSchematic" $ 
                    toSchematic area @?= "###\n###\n###\n"                                    
                    
                , testCase "toSchematic after transformArea" $ 
                    toSchematic (transformArea area) @?= "...\n...\n...\n"

                , testCase "toSchematic after multiTransformArea 2" $ 
                    toSchematic (multiTransformArea area 2) @?= "...\n...\n...\n"      

                , testCase "adj" $
                    length (adjacentAcresOf area (array ! (2,2)) Lumberyard)  @?= 8                    
                ]

        , testGroup "7" $   
            let
                area = fromRight defaultArea $ 
                    fromSchematic 
                        3 
                        3
                        "...\n##|\n..."

                (Area array) = area     
            in
                [ testCase "toSchematic" $ 
                    toSchematic area @?= "...\n##|\n...\n"                                    
                    
                , testCase "toSchematic after transformArea" $ 
                    toSchematic (transformArea area) @?= "...\n.#|\n...\n"
                    
                , testCase "toSchematic after multiTransformArea 2" $ 
                    toSchematic (multiTransformArea area 2) @?= "...\n..|\n...\n"                       

                , testCase "adj" $
                    length (adjacentAcresOf area (array ! (2,2)) Lumberyard)  @?= 1                    
                ]

        , testGroup "8" $   
            [ testCase "scanFile" $ do
                let input1 = "./test/input/AOC18_InitialState.txt" 
                let input2 = "./test/input/AOC18_After10Minutes.txt" 
                let total = 1147

                result1 <- scanFile 10 10 input1
                result2 <- scanFile 10 10 input2

                let area1 = fromRight defaultArea result1
                let area2 = fromRight defaultArea result2

                totalResourceValue (multiTransformArea area1 10) @?= total  
                totalResourceValue area2 @?= total                 

                contents2 <- readFile input2
                contents2 @?= toSchematic (multiTransformArea area1 10)  
            ]
        ]
    ]