module CountStartEnd where
-- NIM / Nama   : 16518189 / M Mirza Fathan Al Arsyad
-- Tanggal      : 25 Februari 2019
-- Topik        : List
-- Deskripsi    : Gabung 2 list dan mengurutkannya

-- DEFINISI DAN SPESIFIKASI LIST
{- type List of Int: [ ] atau [e o List] atau [List o e]  
   Definisi type List of Int
   Basis: List of Int kosong adalah list of Int 
   Rekurens: 
   List tidak kosong dibuat dengan menambahkan sebuah elemen bertype Int di awal 
   sebuah list atau
   dibuat dengan menambahkan sebuah elemen bertype Int di akhir sebuah list -}

-- DEFINISI DAN SPESIFIKASI KONSTRUKTOR
konso :: Int -> [Int] -> [Int]
{- konso e li menghasilkan sebuah list of integer dari e (sebuah integer) dan li 
   (list of integer), dengan e sebagai elemen pertama: e o li -> li' -}
-- REALISASI
konso e li = [e] ++ li

konsDot :: [Int] -> Int -> [Int]
{- konsDot li e menghasilkan sebuah list of integer dari li (list of integer) dan 
   e (sebuah integer), dengan e sebagai elemen terakhir: li o e -> li' -}
-- REALISASI
konsDot li e = li ++ [e]

-- DEFINISI DAN SPESIFIKASI SELEKTOR
-- head :: [Int] -> Int
-- head l menghasilkan elemen pertama list l, l tidak kosong

-- tail :: [Int] -> [Int]
-- tail l menghasilkan list tanpa elemen pertama list l, l tidak kosong

-- last :: [Int] -> Int
-- last l menghasilkan elemen terakhir list l, l tidak kosong

-- init :: [Int] -> [Int]
-- init l menghasilkan list tanpa elemen terakhir list l, l tidak kosong

-- DEFINISI DAN SPESIFIKASI PREDIKAT
isEmpty :: [Int] -> Bool
-- isEmpty l  true jika list of integer l kosong
-- REALISASI
isEmpty l = null l

isOneElmt :: [Int] -> Bool
-- isOneElmt l true jika list of integer l hanya mempunyai satu elemen
-- REALISASI
isOneElmt l = (length l) == 1

-- COUNT START END                                      countStartEnd(l,n)
-- DEFINISI DAN SPESIFIKASI
countStartEnd :: [Int] -> Int -> Int
-- countStartEnd l n mengembalikan hasil penjumlahan n elemen pertama dan terakhir dari l
-- l adalah list integer yang akan dihitung elemennya.
-- n adalah berapa elemen dari awal dan akhir yang akan dihitung
-- prekondisi: n >= 0
-- REALISASI
countStartEnd l n = if (isEmpty l || n == 0) then 0 --Basis
                    else if (isOneElmt l) then head l --Basis
                         else countStartEnd (init (tail l)) (n-1) + (head l + last l) --Recc