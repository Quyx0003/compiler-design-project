module Lang.Simp.IR.DFSpec where 

import Test.Hspec
import Control.Monad.State
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.CFG
import Lang.Simp.IR.DF
import Lang.Simp.IR.Util
import qualified Data.Map as DM
import Debug.Trace


spec :: Spec
spec = do
    describe "df" $ do

        {- 
      1 -> 2 -> 3 -> 4 -> 5 -> 11 -> 12
                     ^    |
                     |    v
                     |    6 -> 7 -> 8 -> 9 -> 10
                     -------------------------/
        -}
        it "test buildDomTree 1" $ 
            let input = DM.fromList [(1 , [2]), (2 , [3]), (3 , [4]), (4 , [5]), (5 , [6, 11]), (6 , [7]), (7 , [8]),
                            (8 , [9]), (9 , [10]), (10 , [4]), (11 , [12])]
                expected = Right (Node 1 [ 
                                    Node 2 [
                                        Node 3 [
                                            Node 4 [
                                                Node 5 [
                                                    Node 6 [
                                                        Node 7 [
                                                            Node 8 [
                                                                Node 9 [
                                                                    Node 10 []
                                                                ]
                                                            ]
                                                        ]
                                                    ],
                                                    Node 11 [Node 12 []]
                                                ]
                                            ]
                                        ]
                                    ]
                                ])
                result = buildDomTree input 1
            in result `shouldBe` expected 

        {-
      1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 13 -> 14
                               ^    |
                               |    v
                               |    8 -> 9 -> 10 -> 11 -> 12
        -}


        it "test buildDomTree 2" $ 
            let input = DM.fromList [(1 , [2]), (2 , [3]), (3 , [4]), (4 , [5]), 
                            (5 , [6]), (6 , [7]), (7 , [8,13]), (8 , [9]), (9 , [10]), 
                            (10 , [11]), (11 , [12]), (12, [6]), (13, [14])]
                
                expected = Right (Node 1 [ 
                                    Node 2 [
                                        Node 3 [
                                            Node 4 [
                                                Node 5 [
                                                    Node 6 [
                                                        Node 7 [
                                                            Node 8 [
                                                                Node 9 [
                                                                    Node 10 [
                                                                        Node 11 [
                                                                            Node 12 [] 
                                                                        ]
                                                                    ]
                                                                ]
                                                            ],
                                                            Node 13 [
                                                                Node 14 []
                                                            ]                                                            
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ])
                result = buildDomTree input 1
            in result `shouldBe` expected 

        {-
      * 1 -> 2 -> 3 -> 4 -> 5 -> 20 -> 21
      *                ^    |
      *                |    v
      *                |    6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 17 -> 18 -> 19 ---------\
      *                |                         ^    |                             |
      *                |                         |    v                             |
      *                |                         |    12 -> 13 -> 14 -> 15 -> 16    |
      *                |                         \----------------------------/     |
      *                \-----------------------------------------------------------/
        -}

        it "test buildDomTree 3" $ 
            let input = DM.fromList [(1, [2]), (2, [3]), (3, [4]),  (4, [5]), 
                                     (5, [6, 20]), (6,[7]), (7, [8]), (8, [9]), 
                                     (9, [10]), (10, [11]), (11, [12, 17]), (12, [13]),
                                     (13, [14]), (14, [15]), (15, [16]), (16, [10]),
                                     (17, [18]), (18, [19]), (19, [4]), (20, [21])]
                
                expected = Right (Node 1 [ 
                                    Node 2 [
                                        Node 3 [
                                            Node 4 [
                                                Node 5 [
                                                    Node 6 [
                                                        Node 7 [
                                                            Node 8 [
                                                                Node 9 [
                                                                    Node 10 [
                                                                        Node 11 [
                                                                            Node 12 [
                                                                                Node 13 [
                                                                                    Node 14 [
                                                                                        Node 15 [
                                                                                            Node 16 []
                                                                                        ]
                                                                                    ]
                                                                                ]
                                                                            ],
                                                                            Node 17 [
                                                                                Node 18 [
                                                                                    Node 19 []
                                                                                ]
                                                                            ] 
                                                                        ]
                                                                    ]
                                                                ]
                                                            ]                                                          
                                                        ]
                                                    ],
                                                    Node 20 [
                                                        Node 21 []
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ])
                result = buildDomTree input 1
            in result `shouldBe` expected 


        it "test postordertrav " $ 
            let dt = Node 1 [ 
                        Node 2 [
                            Node 3 [
                                Node 4 [
                                    Node 5 [
                                        Node 6 [
                                            Node 7 [
                                                Node 8 [
                                                    Node 9 [
                                                        Node 10 [
                                                            Node 11 [
                                                                Node 12 [
                                                                    Node 13 [
                                                                        Node 14 [
                                                                            Node 15 [
                                                                                Node 16 []
                                                                            ]
                                                                        ]
                                                                    ]
                                                                ],
                                                                Node 17 [
                                                                    Node 18 [
                                                                        Node 19 []
                                                                    ]
                                                                ] 
                                                            ]
                                                        ]
                                                    ]
                                                ]                                                          
                                            ]
                                        ],
                                        Node 20 [
                                            Node 21 []
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                result = postOrderTrav dt
                expected = [21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
            in result `shouldBe` expected 

        {- 
      1 -> 2 -> 3 -> 4 -> 5 -> 11 -> 12
                     ^    |
                     |    v
                     |    6 -> 7 -> 8 -> 9 -> 10
                     -------------------------/
        -}
        it "test buildDFT 1" $ 
            let g = DM.fromList [(1 , [2]), (2 , [3]), (3 , [4]), (4 , [5]), (5 , [6, 11]), (6 , [7]), (7 , [8]),
                            (8 , [9]), (9 , [10]), (10 , [4]), (11 , [12])]
                dt = Node 1 [ 
                        Node 2 [
                            Node 3 [
                                Node 4 [
                                    Node 5 [
                                        Node 6 [
                                            Node 7 [
                                                Node 8 [
                                                    Node 9 [
                                                        Node 10 []
                                                    ]
                                                ]
                                            ]
                                        ],
                                        Node 11 [Node 12 []]
                                    ]
                                ]
                            ]
                        ]
                    ]
                result = buildDFT dt g
                expected = DM.fromList [ (1, []), (2, []), (3, []), (4, [4]), 
                                         (5, [4]), (6, [4]), (7, [4]), (8, [4]), 
                                         (9, [4]), (10, [4]), (11, []), (12, [])]
            in result `shouldBe` expected 



        {-
      1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 13 -> 14
                               ^    |
                               |    v
                               |    8 -> 9 -> 10 -> 11 -> 12
        -}


        it "test buildDFT 2" $ 
            let g = DM.fromList [(1 , [2]), (2 , [3]), (3 , [4]), (4 , [5]), 
                            (5 , [6]), (6 , [7]), (7 , [8,13]), (8 , [9]), (9 , [10]), 
                            (10 , [11]), (11 , [12]), (12, [6]), (13, [14])]
                
                dt = Node 1 [ 
                        Node 2 [
                            Node 3 [
                                Node 4 [
                                    Node 5 [
                                        Node 6 [
                                            Node 7 [
                                                Node 8 [
                                                    Node 9 [
                                                        Node 10 [
                                                            Node 11 [
                                                                Node 12 [] 
                                                            ]
                                                        ]
                                                    ]
                                                ],
                                                Node 13 [
                                                    Node 14 []
                                                ]                                                            
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                result = buildDFT dt g
                expected = DM.fromList [ (1, []), (2, []), (3, []), (4, []), (5, []), 
                                         (6, [6]), (7, [6]), (8, [6]), (9, [6]), (10, [6]), (11, [6]), (12, [6]),
                                         (13, []), (14, [])]
            in result `shouldBe` expected 

        {-
      1 -> 2 -> 3 -> 4 -> 5 -> 20 -> 21
                     ^    |
                     |    v
                     |    6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 17 -> 18 -> 19 ---------\
                     |                         ^    |                             |
                     |                         |    v                             |
                     |                         |    12 -> 13 -> 14 -> 15 -> 16    |
                     |                         \----------------------------/     |
                     \-----------------------------------------------------------/
        -}

        it "test buildDFT 3" $ 
            let g = DM.fromList [(1, [2]), (2, [3]), (3, [4]),  (4, [5]), 
                                     (5, [6, 20]), (6,[7]), (7, [8]), (8, [9]), 
                                     (9, [10]), (10, [11]), (11, [12, 17]), (12, [13]),
                                     (13, [14]), (14, [15]), (15, [16]), (16, [10]),
                                     (17, [18]), (18, [19]), (19, [4]), (20, [21])]
                
                dt = Node 1 [ 
                        Node 2 [
                            Node 3 [
                                Node 4 [
                                    Node 5 [
                                        Node 6 [
                                            Node 7 [
                                                Node 8 [
                                                    Node 9 [
                                                        Node 10 [
                                                            Node 11 [
                                                                Node 12 [
                                                                    Node 13 [
                                                                        Node 14 [
                                                                            Node 15 [
                                                                                Node 16 []
                                                                            ]
                                                                        ]
                                                                    ]
                                                                ],
                                                                Node 17 [
                                                                    Node 18 [
                                                                        Node 19 []
                                                                    ]
                                                                ] 
                                                            ]
                                                        ]
                                                    ]
                                                ]                                                          
                                            ]
                                        ],
                                        Node 20 [
                                            Node 21 []
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                result = buildDFT dt g
                expected = DM.fromList [(1,[]),(2,[]),(3,[]),(4,[4]),(5,[4]),(6,[4]),(7,[4]),(8,[4]),(9,[4]),(10,[4,10]),(11,[4,10]),(12,[10]),(13,[10]),(14,[10]),(15,[10]),(16,[10]),(17,[4]),(18,[4]),(19,[4]),(20,[]),(21,[])]
            in result `shouldBe` expected

