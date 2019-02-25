module NbKelipatanX where
-- NIM / Nama   : 16518189 / M Mirza Fathan Al Arsyad
-- Tanggal      : 22 Februari 2019
-- Topik        : Berpikir Rekursif
-- Deskripsi    : Fungsi mencari jumlah kelipatan suatu bilangan dalam suatu selang tutup

-- JUMLAH KELIPATAN X           nbKelipatanX(m,n,x)
-- DEFINISI DAN SPESIFIKASI
nbKelipatanX :: Int -> Int -> Int -> Int
-- { Menerima masukan x,n,m menghasilkan banyaknya kelipatan x pada [m,n] }
-- REALISASI
nbKelipatanX m n x
    | (n == m) = if (mod n x == 0) then 1 else 0 --basis
    | otherwise = if (mod n x == 0) then 1 + (nbKelipatanX m (n-1) x) --recc
                  else (nbKelipatanX m (n-1) x)