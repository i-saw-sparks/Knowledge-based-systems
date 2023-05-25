(deftemplate Cliente
   (slot id (type INTEGER))
   (slot nombre (type STRING))
   (slot correo (type STRING))
   (slot cel (type STRING))
)

(deftemplate Producto
   (slot marca (type STRING))
   (slot modelo (type STRING))
   (slot memoria (type STRING))
   (slot precio (type FLOAT))
   (slot existencia (type INTEGER))
)

(deftemplate TarjetaCredito
   (slot nombre (type STRING))
)

(deftemplate Compra
   (slot cliente-id (type INTEGER))
   (slot producto-marca (type STRING))
   (slot producto-modelo (type STRING))
   (slot tarjeta (type STRING))
   (slot forma-pago (type SYMBOL))
   (slot descuento (type FLOAT))
   (slot meses-sin-intereses (type INTEGER))
)

(deffacts initial-facts
   (Cliente (id 1) (nombre "Juan Perez") (correo "juan@example.com") (cel "555-1234"))
   (Cliente (id 2) (nombre "Maria Lopez") (correo "maria@example.com") (cel "555-5678"))
   (Producto (marca "Apple") (modelo "Apple Watch Series 6") (memoria "32GB") (precio 399.99) (existencia 10))
   (Producto (marca "Samsung") (modelo "Galaxy S21") (memoria "128GB") (precio 899.99) (existencia 5))
   (Producto (marca "Apple") (modelo "iPhone 14") (memoria "256GB") (precio 1099.99) (existencia 3))
   (TarjetaCredito (nombre "Banamex"))
   (TarjetaCredito (nombre "Liverpool VISA"))
)

(defrule iPhone14Banamex
   ?cliente <- (Cliente (id ?id))
   ?producto <- (Producto (marca "Apple") (modelo "iPhone 14"))
   ?tarjeta <- (TarjetaCredito (nombre "Banamex"))
   (not (Compra (cliente-id ?id) (producto-marca "Apple") (producto-modelo "iPhone 14") (tarjeta "Banamex")))
   ?productoCatalogo <- (Producto (marca "Apple") (modelo "iPhone 14") (existencia ?existencia&:(> ?existencia 0)))
   =>
   (bind ?existenciaNueva (- ?existencia 1))
   (modify ?productoCatalogo (existencia ?existenciaNueva))
   (assert (Compra (cliente-id ?id) (producto-marca "Apple") (producto-modelo "iPhone 14") (tarjeta "Banamex") (forma-pago "meses-sin-intereses") (meses-sin-intereses 24)))
)

(defrule TabletSamsungXLiverpool
   ?cliente <- (Cliente (id ?id))
   ?producto <- (Producto (marca "Samsung") (modelo "Tablet SamsungX"))
   ?tarjeta <- (TarjetaCredito (nombre "Liverpool VISA"))
   (not (Compra (cliente-id ?id) (producto-marca "Samsung") (producto-modelo "Tablet SamsungX") (tarjeta "Liverpool VISA")))
   ?productoCatalogo <- (Producto (marca "Samsung") (modelo "Tablet SamsungX") (existencia ?existencia&:(> ?existencia 0)))
   =>
   (bind ?existenciaNueva (- ?existencia 1))
   (modify ?productoCatalogo (existencia ?existenciaNueva))
   (assert (Compra (cliente-id ?id) (producto-marca "Samsung") (producto-modelo "Tablet SamsungX") (tarjeta "Liverpool VISA") (forma-pago "meses-sin-intereses") (meses-sin-intereses 12)))
)

(defrule DescuentoFundaMica
   ?cliente <- (Cliente (id ?id))
   ?compra <- (Compra (cliente-id ?id) (producto-marca ?marca) (producto-modelo ?modelo) (forma-pago ?forma-pago))
   ?producto <- (Producto (marca ?marca) (modelo ?modelo) (existencia ?existencia&:(> ?existencia 0)))
   (not (Compra (cliente-id ?id) (producto-marca ?marca) (producto-modelo ?modelo) (forma-pago "descuento")))
   =>
   (bind ?existenciaNueva (- ?existencia 1))
   (modify ?producto (existencia ?existenciaNueva))
   (assert (Compra (cliente-id ?id) (producto-marca ?marca) (producto-modelo ?modelo) (forma-pago "descuento") (descuento 0.15)))
)

(defrule DescuentoVales
   ?cliente <- (Cliente (id ?id))
   ?compra1 <- (Compra (cliente-id ?id) (producto-marca "Apple") (producto-modelo "Apple Watch Series 6") (forma-pago ?forma-pago1))
   ?compra2 <- (Compra (cliente-id ?id) (producto-marca "Apple") (producto-modelo "iPhone 14") (forma-pago ?forma-pago2))
   ?producto1 <- (Producto (marca "Apple") (modelo "Apple Watch Series 6") (existencia ?existencia1&:(> ?existencia1 0)))
   ?producto2 <- (Producto (marca "Apple") (modelo "iPhone 14") (existencia ?existencia2&:(> ?existencia2 0)))
   (not (Compra (cliente-id ?id) (producto-marca "Apple") (producto-modelo "Apple Watch Series 6") (forma-pago "descuento")))
   =>
   (bind ?existenciaNueva1 (- ?existencia1 1))
   (bind ?existenciaNueva2 (- ?existencia2 1))
   (modify ?producto1 (existencia ?existenciaNueva1))
   (modify ?producto2 (existencia ?existenciaNueva2))
   (bind ?total (+ (* ?existencia1 ?precio1) (* ?existencia2 ?precio2)))
   (bind ?vales (/?total 1000 100))
   (assert (Compra (cliente-id ?id) (producto-marca "Apple") (producto-modelo "Apple Watch Series 6") (forma-pago "descuento") (descuento ?vales)))
)
