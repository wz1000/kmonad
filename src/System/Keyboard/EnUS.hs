{-# LANGUAGE QuasiQuotes #-}
-- |

module System.Keyboard.EnUS where

import System.Keyboard.Prelude

import Text.RawString.QQ
import qualified RIO.Text as T

-- | org-mode table representation of the enUS table.
--
-- TODO: This could probably be TH, right?
tableEnUSText :: Text
tableEnUSText = T.drop 1 $ [r|
| core name | shifted | linux |         mac | windows | description          |
|-----------+---------+-------+-------------+---------+----------------------|
| q         | Q       |  0x10 | 0x700000014 |    0x51 | letter Q             |
| w         | W       |  0x11 | 0x70000001A |    0x57 | letter W             |
| e         | E       |  0x12 | 0x700000008 |    0x45 | letter E             |
| r         | R       |  0x13 | 0x700000015 |    0x52 | letter R             |
| t         | T       |  0x14 | 0x700000017 |    0x54 | letter T             |
| y         | Y       |  0x15 | 0x70000001C |    0x59 | letter Y             |
| u         | U       |  0x16 | 0x700000018 |    0x55 | letter U             |
| i         | I       |  0x17 | 0x70000000C |    0x49 | letter I             |
| o         | O       |  0x18 | 0x700000012 |    0x4F | letter O             |
| p         | P       |  0x19 | 0x700000013 |    0x50 | letter P             |
| a         | A       |  0x1E | 0x700000004 |    0x41 | letter A             |
| s         | S       |  0x1F | 0x700000016 |    0x53 | letter S             |
| d         | D       |  0x20 | 0x700000007 |    0x44 | letter D             |
| f         | F       |  0x21 | 0x700000009 |    0x46 | letter F             |
| g         | G       |  0x22 | 0x70000000A |    0x47 | letter G             |
| h         | H       |  0x23 | 0x70000000B |    0x48 | letter H             |
| j         | J       |  0x24 | 0x70000000D |    0x4A | letter J             |
| k         | K       |  0x25 | 0x70000000E |    0x4B | letter K             |
| l         | L       |  0x26 | 0x70000000F |    0x4C | letter L             |
| z         | Z       |  0x2C | 0x70000001D |    0x5A | letter Z             |
| x         | X       |  0x2D | 0x70000001B |    0x58 | letter X             |
| c         | C       |  0x2E | 0x700000006 |    0x43 | letter C             |
| v         | V       |  0x2F | 0x700000019 |    0x56 | letter V             |
| b         | B       |  0x30 | 0x700000005 |    0x42 | letter B             |
| n         | N       |  0x31 | 0x700000011 |    0x4E | letter N             |
| m         | M       |  0x32 | 0x700000010 |    0x4D | letter M             |
| 1         | !       |  0x02 | 0x70000001E |    0x31 | number 1             |
| 2         | @       |  0x03 | 0x70000001F |    0x32 | number 2             |
| 3         | #       |  0x04 | 0x700000020 |    0x33 | number 3             |
| 4         | $       |  0x05 | 0x700000021 |    0x34 | number 4             |
| 5         | %       |  0x06 | 0x700000022 |    0x35 | number 5             |
| 6         | ^       |  0x07 | 0x700000023 |    0x36 | number 6             |
| 7         | &       |  0x08 | 0x700000024 |    0x37 | number 7             |
| 8         | *       |  0x09 | 0x700000025 |    0x38 | number 8             |
| 9         | lparen  |  0x0A | 0x700000026 |    0x39 | number 9             |
| 0         | rparen  |  0x0B | 0x700000027 |    0x30 | number 0             |
| -         | under   |  0x0C | 0x700000035 |    0xBD | minus                |
| esc       |         |  0x01 | 0x700000029 |    0x1B | Escape               |
| ralt      |         |  0x64 | 0x7000000E6 |    0xA5 | Right alt            |
| lsft      |         |  0x2A | 0x7000000E1 |    0xA0 | Left shift           |
| fn        |         |       | 0x700000003 |         | Mac Fn               |
|]
