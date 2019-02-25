module InfoNilaiMaks where
-- NIM / Nama   : 16518189 / M Mirza Fathan Al Arsyad
-- Tanggal      : 22 Februari 2019
-- Topik        : List
-- Deskripsi    : Mencari nilai maksimum dalam suatu list

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


-- NILAI MAKSIMUM                               infoNilaiMaks(l)
-- DEFINISI DAN SPESIFIKASI
infoNilaiMaks :: [Int] -> (Int,Int)
-- { infoNilaiMaks menghitung banyak siswa yang lulus dan nilai tertinggi dari siswa di 
-- kelas dalam bentuk tuple dari list nilai yang diberikan }
nilaiMaks :: [Int] -> Int
-- { keluaran berupa nilai maksimum dalam list }
lulus :: [Int] -> Int
-- { keluaran berupa jumlah diatas 75 }
maks :: Int -> Int -> Int
-- {mencari maksimum dari dua nilai}
-- REALISASI
maks a b = if(a >= b) then a else b
nilaiMaks l = if isOneElmt l then (head l) --basis
              else (maks (head l) (nilaiMaks (tail (l)))) --recc
lulus l
    | isOneElmt l = if head l >= 75 then 1 else 0 --basis
    | otherwise = if head l >= 75 then 1 + (lulus (tail l)) else lulus (tail l) --recc
infoNilaiMaks l = (lulus l, nilaiMaks l)

