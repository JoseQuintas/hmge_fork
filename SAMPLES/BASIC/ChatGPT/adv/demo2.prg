#include "minigui.ch"

function Main()
   DEFINE WINDOW WinMain ;
      AT 0,0 ;
      WIDTH 800 ;
      HEIGHT 600 ;
      TITLE "Inventory Management System" ;
      MAIN ;
      ON INIT OnInit()

   @ 20,20 TAB TabMain ;
      WIDTH 750 ;
      HEIGHT 500

      // Products Tab
      PAGE 'Products'

      @ 40,20 GRID GrdProducts ;
         WIDTH 700 ;
         HEIGHT 400 ;
         HEADERS { "Product ID", "Product Name", "Quantity", "Price" } ;
         WIDTHS { 100, 250, 100, 100 } ;
         ITEMS {{ 1, "Product A", 50, 10.00 }, { 2, "Product B", 30, 15.00 }}

      @ 460,620 BUTTON BtnAddProduct CAPTION "Add" ACTION AddProduct()
      @ 460,520 BUTTON BtnDeleteProduct CAPTION "Delete" ACTION DeleteProduct("GrdProducts")

      END PAGE

      // Suppliers Tab
      PAGE 'Suppliers'

      @ 40,20 GRID GrdSuppliers ;
         WIDTH 700 ;
         HEIGHT 400 ;
         HEADERS { "Supplier ID", "Supplier Name", "Contact" } ;
         WIDTHS { 100, 250, 150 } ;
         ITEMS {{ 1, "Supplier A", "123-456-7890" }, { 2, "Supplier B", "098-765-4321" }}

      @ 460,620 BUTTON BtnAddSupplier CAPTION "Add" ACTION AddSupplier()
      @ 460,520 BUTTON BtnDeleteSupplier CAPTION "Delete" ACTION DeleteSupplier("GrdSuppliers")

      END PAGE

      // Orders Tab
      PAGE 'Orders'

      @ 40,20 GRID GrdOrders ;
         WIDTH 700 ;
         HEIGHT 400 ;
         HEADERS { "Order ID", "Product ID", "Quantity", "Date" } ;
         WIDTHS { 100, 100, 100, 150 } ;
         ITEMS {{ 1, 1, 10, "2024-07-20" }, { 2, 2, 5, "2024-07-21" }}

      @ 460,620 BUTTON BtnAddOrder CAPTION "Add" ACTION AddOrder()
      @ 460,520 BUTTON BtnDeleteOrder CAPTION "Delete" ACTION DeleteOrder("GrdOrders")

      END PAGE

	END TAB

   // Progress Bar
   @ 10,320 PROGRESSBAR PrgLoading ;
      WIDTH 450 ;
      HEIGHT 25 ;
      RANGE 0,100 ;
      VALUE 0

   END WINDOW

   ACTIVATE WINDOW WinMain

return nil

function OnInit()
   // Simulate data loading
   LoadData("PrgLoading")
return nil

function LoadData(PrgLoading)
   LOCAL i
   FOR i := 10 TO 100 STEP 10
      WinMain.(PrgLoading).Value := i
      Sleep(1) // Simulate loading
   NEXT
   WinMain.(PrgLoading).Hide
return nil

PROCEDURE Sleep( n )

   n += Seconds()
   WHILE Seconds() < n
      DO EVENTS
   ENDDO

RETURN

function AddProduct()
   // Code for adding a product
return nil

function DeleteProduct(GrdProducts)
   LOCAL nRow := WinMain.(GrdProducts).Value
   IF nRow > 0
      WinMain.(GrdProducts).DeleteItem(nRow)
      WinMain.(GrdProducts).Refresh()
   ELSE
      MsgInfo("No row selected!")
   ENDIF
return nil

function AddSupplier()
   // Code for adding a supplier
return nil

function DeleteSupplier(GrdSuppliers)
   LOCAL nRow := WinMain.(GrdSuppliers).Value
   IF nRow > 0
      WinMain.(GrdSuppliers).DeleteItem(nRow)
      WinMain.(GrdSuppliers).Refresh()
   ELSE
      MsgInfo("No row selected!")
   ENDIF
return nil

function AddOrder()
   // Code for adding an order
return nil

function DeleteOrder(GrdOrders)
   LOCAL nRow := WinMain.(GrdOrders).Value
   IF nRow > 0
      WinMain.(GrdOrders).DeleteItem(nRow)
      WinMain.(GrdOrders).Refresh()
   ELSE
      MsgInfo("No row selected!")
   ENDIF
return nil
