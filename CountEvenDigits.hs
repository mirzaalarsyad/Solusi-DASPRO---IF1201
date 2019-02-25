module CountEvenDigits where
-- NIM / Nama   : 16518189 / M Mirza Fathan Al Arsyad
-- Tanggal      : 22 Februari 2019
-- Topik        : Berpikir Rekursif
-- Deskripsi    : Menghitung jumlah digit genap dalam suatu bilangan bulat

-- HITUNG DIGIT GENAP                             countEvenDigits(n)
-- DEFINISI DAN SPESIFIKASI
countEvenDigits :: Int -> Int
-- { Menerima masukan 1 nilai integer positif. Realisasi fungsi countEvenDigits(n) menghasilkan banyaknya digit bilangan tersebut yang genap (0, 2, 4, 6, atau 8) }
-- REALISASI
countEvenDigits n
    | div n 10 == 0 = if (mod n 2 == 0) then 1 else 0 --basis
    | otherwise =   if (mod n 2 == 0) then 1 + (countEvenDigits (div n 10)) --recc
                    else (countEvenDigits (div n 10))