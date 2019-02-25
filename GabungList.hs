module GabungList where

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

-- GABUNG LIST                                      gabungList(l1,l2)
-- DEFINISI DAN SPESIFIKASI
gabungList :: [Int] -> [Int] -> [Int]
-- gabungList menerima masukan dua buah list of integer l1 dan l2 dan mengembalikan
-- sebuah list of integer l3. l1 adalah list dengan bilangan sembarang dan l2 adalah
-- list yang sudah terurut serta l3 adalah gabungan dari kedua list dengan isi dari
-- list tersebut sudah terurut tidak mengecil (membesar atau sama).
inputList :: Int -> [Int] -> [Int]
-- menerima masukan satu integer untuk dimasukkan kedalam suatu list
-- REALISASI
gabungList l1 l2 = if (isEmpty l1) then l2 -- basis
                   else  gabungList (tail l1) (inputList (head l1) l2)--recc
inputList n l = if (isEmpty l) then konso n l --basis
                else if (n <= head l) then konso n l
                     else konso (head l) (inputList n (tail l)) -- recc