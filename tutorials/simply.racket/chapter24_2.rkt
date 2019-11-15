#|
24.2  Make a table of tax and tip amounts for a range of possible costs at a restaurant. 
Column a should contain the pre-tax amounts, starting at 50 cents and increasing by 50 cents per row. 
(Do this without entering each row separately!) 
Column b should compute the tax, based on your state's tax rate. 
Column c should compute the 15% tip. 
Column d should add columns a through c to get the total cost of the meal. 
Column e should contain the same total cost, rounded up to the next whole dollar amount. 
|#

(put "Pre-tax" a1)
(put "Tax" b1)
(put "Tip" c1)
(put "Total Cost" d1)
(put "Rounded Up" e1)

(put 0.50 a2); base
(put 1.1 b2) ; tax

(put (+ (cell a <1) 0.5) a)
(put (+ 0 (cell b <1)) b)
(put (* 0.15 (cell a <0)) c)
(put (+ (cell a <0) (cell b <0) (cell c <0)) d)
(put (ceiling (cell d <0)) e)

