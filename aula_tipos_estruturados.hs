type Hora = (Int, Int, Int)
--verificar se a hora é valida
horaValida :: Hora -> Bool
horaValida(h,m,s)
    |h >=0 && h < 24 && m >=0 && m < 60 && s >= 0 && s < 60 = True
    |otherwise = False


-- Converter horas para segundos
horaParaSegundos :: Hora -> Int
horaParaSegundos (h, m, s) = (h * 3600) + (m *60) + s

--converter segundos para horas
segundosEmHora :: Int -> Double
segundosEmHora n = fromIntegral n / 3600

--versao do LF
segundosEmHoraLf :: Int -> Hora
segundosEmHoraLf s
    |s < 60 = (0, 0, s)
    |s < 3600 = (0, (div s 60), (mod s 60))
    |otherwise = (hora, minutos, segundos)
    
    where
        hora = (div s 3600)
        modMinutos = (mod s 3600)
        minutos = (div modMinutos 60)
        segundos = (mod modMinutos 60)




