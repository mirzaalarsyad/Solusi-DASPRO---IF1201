module HapusElemenX where
-- NIM / Nama   : 16518189 / M Mirza Fathan Al Arsyad
-- Tanggal      : 22 Februari 2019
-- Topik        : List
-- Deskripsi    : Hapus Elemen X dari suatu list

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


-- HAPUS ELEMEN X                          hapusElemenX(l,x)
-- DEFINISI DAN SPESIFIKASI
hapusElemenX :: [Int] -> Int -> [Int]
-- { hapusElemenX menghapus semua kemunculan x pada l, sehingga kembalian dari fungsi adalah list of integer (l) yang tidak memiliki elemen x}
-- { elementSetelahX membuat list berisikan elemen setelah x pada list input}
-- REALISASI
hapusElemenX l x = if (isEmpty l) then l -- basis
                   else if (head l /= x) then  konso (head l) (hapusElemenX (tail l) x)
                        else hapusElemenX (tail l) x --recc
 