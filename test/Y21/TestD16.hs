module Y21.TestD16 where

import           Test.Hspec

import           Parser
import           Y21.D16

spec :: Spec
spec = do
    it "packetPadded" $ do
        -- 110 100 10111 11110 00101 000
        -- VVV TTT AAAAA AAAAA AAAAA
        -- ^^^                                                              - 3 - version (6)
        --     ^^^                                                          - 3 - type id (4 => literal)
        --         ^^^^^                                                    - 4 - (not last)
        --               ^^^^^                                              - 4 - (not last)
        --                     ^^^^^                                        - 4 - (last)
        --                           ^^^                                    - 3 - padding
        parseOrDie packetPadded (hex2bin "d2fe28") `shouldBe`
            Packet 6 (Literal 0b011111100101) []

        -- 001 110 0 000000000011011 11010001010 0101001000100100 0000000
        -- VVV TTT I LLLLLLLLLLLLLLL VVVTTTAAAAA VVVTTTBBBBBBBBBB
        -- ^^^                                                              - 3   - version (6)
        --     ^^^                                                          - 3   - type id (not 4 => operator)
        --         ^                                                        - 1   - length type (0 => 15 bits)
        --           ^^^^^^^^^^^^^^^                                        - 15? - 27 -- total length of sub-packets
        --                           ^^^^^^^^^^^                            - 11? - packet A
        --                                       ^^^^^^^^^^^^^^^^           - 16? - packet B
        --                                                        ^^^^^^^^  - 7   - padding
        parseOrDie packetPadded (hex2bin "38006f45291200") `shouldBe`
            Packet 1 Lesser
                [ Packet 6 (Literal 10) []
                , Packet 2 (Literal 20) []
                ]

        -- 111 011 1 00000000011 01010000001 10010000010 00110000011 00000
        -- VVV TTT I LLLLLLLLLLL VVVTTTAAAAA VVVTTTBBBBB VVVTTTCCCCC
        -- ^^^                                                              - 3   - version (6)
        --     ^^^                                                          - 3   - type id (not 4 => operator)
        --         ^                                                        - 1   - length type (1 => 11 bits)
        --           ^^^^^^^^^^^                                            - 11? - 3 -- number of sub-packets
        --                       ^^^^^^^^^^^                                -     - packet A (literal 1)
        --                                   ^^^^^^^^^^^                    -     - packet B (literal 2)
        --                                               ^^^^^^^^^^^        -     - packet C (literal 3)
        --                                                           ^^^^^  - 5   - padding
        parseOrDie packetPadded (hex2bin "ee00d40c823060") `shouldBe`
            Packet 7 Maximum
                [ Packet 2 (Literal 1) []
                , Packet 4 (Literal 2) []
                , Packet 1 (Literal 3) []
                ]

    it "sumOfVersions" $ do
        solve1 "8A004A801A8002F478"             `shouldBe` 16
        solve1 "620080001611562C8802118E34"     `shouldBe` 12
        solve1 "C0015000016115A2E0802F182340"   `shouldBe` 23
        solve1 "A0016C880162017C3686B18A3D4780" `shouldBe` 31
