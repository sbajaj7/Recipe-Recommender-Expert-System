;; main Module

(deftemplate question
    (slot text)
    (slot type)
    (slot ident))

(deftemplate answer
    (slot ident)
    (slot text))

(deftemplate recommendation
    (slot recipe)
    (slot explanation))

(deffacts question-data
    "The questions the system can ask."
      (question (ident courseselection) (type string)
            	(text " What would you like to have?(starter or maincourse or desert)"))
    
      (question (ident cottagecheese) (type yes-no)
            	(text " Do you have cottage cheese?"))
      (question (ident yoghurt) (type yes-no)
            	(text " Do you have yoghurt?"))
      (question (ident vinegar) (type yes-no)
            	(text " Do you have vinegar?"))
    
      (question (ident cauliflower) (type yes-no)
            	(text " Do you have cauliflower?"))
      (question (ident leek) (type yes-no)
            	(text " Do you have leek?"))
    
    
      (question (ident potatoes) (type yes-no)
            	(text " Do you have potatoes?"))
      (question (ident paprika) (type yes-no)
            	(text " Do you have paprika?"))
      (question (ident oil) (type yes-no)
            	(text " Do you have oil?"))
    
      (question (ident chickpeas) (type yes-no)
            	(text " Do you have chickpeas?"))
      (question (ident spinach) (type yes-no)
            	(text " Do you have spinach?"))
      (question (ident garlic) (type yes-no)
	            (text " Do you have garlic?"))
        
	  (question (ident pasta) (type yes-no)
	            (text " Do you have pasta?"))
	  (question (ident cheese) (type yes-no)
	            (text " Do you have cheese?"))  
	  (question (ident cream) (type yes-no)
	            (text " Do you have cream?"))
    
      
      (question (ident sugar) (type yes-no)
            	(text " Do you have sugar?"))
      (question (ident eggs) (type yes-no)
            	(text " Do you have eggs?"))
      (question (ident peanutbutter) (type yes-no)
            	(text " Do you have Peanut Butter?"))
      (question (ident butter) (type yes-no)
            	(text " Do you have Butter?"))
      (question (ident chocolate) (type yes-no)
            	(text " Do you have Chocolate?")))

(defglobal ?*crlf* = "
")

;; ask Module

(defmodule ask)

(deffunction is-of-type (?answer ?type)
    "Check that the answer has the right form"
    (if (eq ?type yes-no) then
        (return (or (eq ?answer yes) (eq ?answer no)))
    elif (eq ?type number) then
        (return (numberp ?answer))
    else (return (> (str-length ?answer) 0))))

(deffunction ask-user (?question ?type)
  "Ask a question, and return the answer"
  (bind ?answer "")
  (while (not (is-of-type ?answer ?type)) do
         (printout t ?question " ")
         (if (eq ?type yes-no) then
           (printout t "(yes or no) "))
         (bind ?answer (read)))
  (return ?answer))

   
(defrule ask::ask-question-by-id
  "Given the identifier of a question, ask it and assert the answer"
  (declare (auto-focus TRUE))
  (MAIN::question (ident ?id) (text ?text) (type ?type))
  (not (MAIN::answer (ident ?id)))
  ?ask <- (MAIN::ask ?id)
  =>
  (bind ?answer (ask-user ?text ?type))
  (assert (answer (ident ?id) (text ?answer)))
  (retract ?ask)
  (return))

;; interview Module
(defmodule interview)

(defrule request-course
;; If the user has starter or maincourse or desert...
  =>
  (assert (ask courseselection)))

(defrule bad-choice
   ?choice <- (answer (ident courseselection) (text ?cs&~starter&~maincourse&~desert))
   =>
   (retract ?choice)
   (assert (ask courseselection))
   (printout t " You have chosen incorrect option. Choose anything between the following 3 values: starter or maincourse or desert." crlf))

(defrule request-cottagecheese
  (answer (ident courseselection) (text ?cs&:(eq ?cs starter)))
  =>
  (assert (ask cottagecheese)))

(defrule request-yoghurt
;; If the user has cottage cheese...
  (answer (ident cottagecheese) (text ?cc&:(eq ?cc yes)))
  =>
  (assert (ask yoghurt)))

(defrule request-vinegar
;; If the user has yoghurt...
  (answer (ident yoghurt) (text ?y&:(eq ?y yes)))
  =>
  (assert (ask vinegar)))

(defrule request-cauliflower
  (answer (ident courseselection) (text ?cs&:(eq ?cs starter)))
  =>
  (assert (ask cauliflower)))

(defrule request-leek
;; If the user has cauliflower...
  (answer (ident cauliflower) (text ?ca&:(eq ?ca yes)))
  =>
  (assert (ask leek)))


(defrule request-maincourse-desert1
    
  ( and(answer (ident cottagecheese) (text ?cgc&:(eq ?cgc no))) 
       (answer (ident cauliflower) (text ?cau&:(eq ?cau no))) 	
  )
     
  =>
  (printout t "You do not have enough ingredients to cook any starter. Please try again or choose maincourse or desert." crlf))

(defrule request-maincourse-desert2
    
  ( and(answer (ident cauliflower) (text ?cgc&:(eq ?cgc no))) 
    (or(answer (ident yoghurt) (text ?cau&:(eq ?cau no))) 	
       (answer (ident vinegar) (text ?cau&:(eq ?cau no))) )
  )
     
  =>
  (printout t "You do not have enough ingredients to cook any starter. Please try again or choose maincourse or desert." crlf))

(defrule request-maincourse-desert3
    
  ( and(answer (ident leek) (text ?cgc&:(eq ?cgc no))) 
    (or(answer (ident yoghurt) (text ?cau&:(eq ?cau no))) 	
       (answer (ident vinegar) (text ?cau&:(eq ?cau no))) )
  )
     
  =>
  (printout t "You do not have enough ingredients to cook any starter. Please try again or choose maincourse or desert." crlf))


(defrule request-potatoes
  (answer (ident courseselection) (text ?cs&:(eq ?cs maincourse)))
  =>
  (assert (ask potatoes)))

(defrule request-paprika
;; If the user has potatoes...
  (answer (ident potatoes) (text ?t&:(eq ?t yes)))
  =>
  (assert (ask paprika)))

(defrule request-chickpeas
  (answer (ident courseselection) (text ?cs&:(eq ?cs maincourse)))
  =>
  (assert (ask chickpeas)))

(defrule request-spinach
;; If the user has chickpeas...
  (answer (ident garlic) (text ?c&:(eq ?c yes)))
  =>
  (assert (ask spinach)))

(defrule request-garlic
;; If the user has potatoes or chickpeas...
   (answer (ident chickpeas) (text ?s&:(eq ?s yes)))
  =>
  (assert (ask garlic)))


(defrule request-pasta
   (answer (ident courseselection) (text ?cs&:(eq ?cs maincourse)))
  =>
  (assert (ask pasta)))

(defrule request-cheese
(answer (ident pasta) (text ?p&:(eq ?p yes)))
  =>
  (assert (ask cheese)))

(defrule request-cream
(answer (ident cheese) (text ?ch&:(eq ?ch yes)))
  =>
  (assert (ask cream)))
    
    

(defrule request-starter-desert1
    
  ( and(answer (ident chickpeas) (text ?c1&:(eq ?c1 no))) 
       (answer (ident pasta) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident potatoes) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))
    

(defrule request-starter-desert2
    
  ( and(answer (ident cheese) (text ?p1&:(eq ?p1 no)))
       (answer (ident chickpeas) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident potatoes) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))

(defrule request-starter-desert3
    
  ( and(answer (ident cream) (text ?p1&:(eq ?p1 no)))
       (answer (ident chickpeas) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident potatoes) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))


(defrule request-starter-desert4
    
  ( and(answer (ident spinach) (text ?c1&:(eq ?c1 no))) 
       (answer (ident pasta) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident potatoes) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))

(defrule request-starter-desert5
    
  ( and(answer (ident spinach) (text ?c1&:(eq ?c1 no))) 
       (answer (ident cheese) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident potatoes) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))

(defrule request-starter-desert6
    
  ( and(answer (ident chickpeas) (text ?c1&:(eq ?c1 no))) 
       (answer (ident pasta) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident paprika) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))


(defrule request-starter-desert7
    
  ( and(answer (ident spinach) (text ?c1&:(eq ?c1 no))) 
       (answer (ident cheese) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident paprika) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))


(defrule request-starter-desert8
    
  ( and(answer (ident garlic) (text ?c1&:(eq ?c1 no))) 
       (answer (ident cheese) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident paprika) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))


(defrule request-starter-desert9
    
  ( and(answer (ident garlic) (text ?c1&:(eq ?c1 no))) 
       (answer (ident cream) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident paprika) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))


(defrule request-starter-desert10
    
  ( and(answer (ident garlic) (text ?c1&:(eq ?c1 no))) 
       (answer (ident cream) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident potatoes) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))


(defrule request-starter-desert11
    
  ( and(answer (ident garlic) (text ?c1&:(eq ?c1 no))) 
       (answer (ident cheese) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident potatoes) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))

(defrule request-starter-desert12
    
  ( and(answer (ident cream) (text ?p1&:(eq ?p1 no)))
       (answer (ident chickpeas) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident paprika) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))

(defrule request-starter-desert13
    
  ( and(answer (ident cheese) (text ?p1&:(eq ?p1 no)))
       (answer (ident chickpeas) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident paprika) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))

(defrule request-starter-desert14
    
  ( and(answer (ident garlic) (text ?c1&:(eq ?c1 no))) 
       (answer (ident pasta) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident potatoes) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))

(defrule request-starter-desert15
    
  ( and(answer (ident spinach) (text ?c1&:(eq ?c1 no))) 
       (answer (ident cream) (text ?p1&:(eq ?p1 no))) 	
       (answer (ident potatoes) (text ?p1&:(eq ?p1 no)))
  )
     
  =>
  (printout t " You do not have enough ingredients to cook anything in maincourse. Please try again or choose starter or desert." crlf))

(defrule request-sugar
;; If the user chooses desert...
  (answer (ident courseselection) (text ?cs&:(eq ?cs desert)))
  =>
  (assert (ask sugar)))

(defrule request-eggs
;; If the user has sugar...
  (answer (ident sugar) (text ?sg&:(eq ?sg yes)))
  =>
  (assert (ask eggs)))
    
(defrule request-peanutbutter
  (answer (ident eggs) (text ?eg&:(eq ?eg yes)))
  =>
  (assert (ask peanutbutter)))
    
(defrule request-chocolate
  (answer (ident eggs) (text ?eg&:(eq ?eg yes)))
  =>
  (assert (ask chocolate)))
    
(defrule request-butter
  (answer (ident chocolate) (text ?choc&:(eq ?choc yes)))
  =>
  (assert (ask butter)))

(defrule request-starter-maincours1
    
  ( or(answer (ident sugar) (text ?sgr&:(eq ?sgr no))) 
       (answer (ident eggs) (text ?egg&:(eq ?egg no))) 	
  )
     
  =>
  (printout t " You do not have enough ingredients to cook any desert. Please try again or choose starter or maincourse." crlf))

(defrule request-starter-maincourse2
    
  (and (answer (ident sugar) (text ?sg1&:(eq ?sg1 yes))) 
       (answer (ident eggs) (text ?egg&:(eq ?egg yes))) 
	   (answer (ident chocolate) (text ?ch1&:(eq ?ch1 no)))
       (answer (ident peanutbutter) (text ?pb1&:(eq ?pb1 no)))         	
  )
     
  =>
  (printout t " You do not have enough ingredients to cook any desert. Please try again or choose starter or maincourse." crlf))

(defrule request-starter-maincourse3
    
  (and (answer (ident sugar) (text ?sg1&:(eq ?sg1 yes))) 
       (answer (ident eggs) (text ?egg&:(eq ?egg yes))) 
	   (answer (ident butter) (text ?b1&:(eq ?b1 no)))
       (answer (ident peanutbutter) (text ?pb1&:(eq ?pb1 no)))         	
  )
     
  =>
  (printout t " You do not have enough ingredients to cook any desert. Please try again or choose starter or maincourse." crlf))


;; startup Module

(defmodule startup)

(defrule print-banner
    =>
    (printout t "Please enter your name and then press enter key> ")
    (bind ?name (read))
    (printout t crlf " " crlf)
    (printout t " Hello, " ?name "." crlf)
    (printout t " Welcome to the Recipe Recommender" crlf)
    (printout t " Please answer the following questions and we will tell you what meal to cook." crlf)
    (printout t " " crlf crlf))

;; recommend Module

(defmodule recommend)

(defrule recipe-paneertikka
    (answer (ident cottagecheese) (text yes))
    (answer (ident yoghurt) (text yes))
    (answer (ident vinegar) (text yes))
    =>
    (assert
        (recommendation (recipe Paneer-Tikka) (explanation " Mix the masalas in the yoghurt and leave for 20-30 minutes.
 Place a saucer like plate, greased in a bigger plate. 
 Arrange the paneer pieces along the edge and cook, uncovered on HI for 4 minutes. 
 Turnover, brush with a little oil and cook again on HI for 4 minutes and serve.
            
 Video Link - https://www.youtube.com/watch?v=abIuyROpyuE
                "))))

(defrule recipe-leek-cauliflower-soup
    (answer (ident leek) (text yes))
    (answer (ident cauliflower) (text yes))
    =>
    (assert
        (recommendation (recipe Leek-Cauliflower-Soup) (explanation " Roast all three of this soup’s star ingredients to bring out their sweeter side.
 Make it: In a large roasting pan, toss cleaned leeks, cauliflower florets and apple chunks with olive oil, salt and pepper. Roast until tender then blend with stock. Garnish with sage.
 Tip: Leeks often have gritty soil trapped between the layers. To remove, add your chopped leeks to a bowl of cold water and swirl them around a few times. Lift out onto a work surface, discard the gritty water and repeat a few times until no grit remains in the bowl.
               
 Video Link - https://www.youtube.com/watch?v=ZhoySQtR8ZM
                "))))

(defrule recipe-skilletchickpeas
    (answer (ident chickpeas) (text yes))
    (answer (ident spinach) (text yes))
    (answer (ident garlic) (text yes))
    =>
    (assert
        (recommendation (recipe Skillet-Chickpeas) (explanation " Heat the oil and garlic over high heat for two minutes. 
 Add the spinach, chickpeas and cumin to the skillet. 
 Stir constantly until the spinach fully wilts and the chickpeas cook through. 
 Top with salt and pepper. 
 Enjoy on its own, or serve with crusty bread!
                
 Video Link - https://www.youtube.com/watch?v=KWXgDyJXAzg
                "))))

(defrule recipe-bakedpotatosticks
    (answer (ident potatoes) (text yes))
    (answer (ident paprika) (text yes))
    =>
    (assert
        (recommendation (recipe Baked-Potato-Paprika-Sticks) (explanation " Preheat oven to 400 degrees F (200 degrees C). 
 Spray a baking sheet with cooking spay or vegetable oil.
 In a large bowl, mix oil and paprika. Add potato sticks, and stir by hand to coat. Place on the prepared baking sheet.
 Bake 40 minutes in the preheated oven. Best eaten at room temperature.
                
 Video Link - https://www.youtube.com/watch?v=jUgzT6a--PM
                "))))

(defrule recipe-MacnCheese
    (answer (ident pasta) (text yes))
    (answer (ident cheese) (text yes))
    (answer (ident cream) (text yes))
    =>
    (assert
        (recommendation (recipe Mac-n-Cheese) (explanation " Gather ingredients.
 Cook pasta just until it’s a bit undercooked. Should be a bit firm towards center. Drain pasta in colander. Do NOT rinse! Doing so will wash away the starch needed to bind.
 Pour container (16oz) of Heavy Cream (you could use light cream, if you want) into pot on low/simmer until warmed through.
 Once heavy cream is warmed, add entire bag (8oz = 2cups) of shredded cheese. 
 Stir heavy cream & cheese mixture on low until combined and smooth texture.
 Stir noodles into pot of cheese sauce. Mix well and simmer until its nicely mixed up (just a few minutes). 
 Serve well.
                
 Video Link - https://www.youtube.com/watch?v=5HkPrRpqOEo 
"))))

(defrule recipe-Potato-Chickpeas-Curry
    (answer (ident potatoes) (text yes))
    (answer (ident chickpeas) (text yes))
    (answer (ident garlic) (text yes))
    =>
    (assert
        (recommendation (recipe Potato-Chickpeas-Garlic-Curry) (explanation " Wash the dried Chickpeas under running cold water till water runs clear.
 Soak them in enough water for at least 8 hours or overnight. After soaking time, they will get doubled in size. Discard the soaking water.
 Also boil the potato in pressure cooker or microwave. You can use leftover boiled potato as well.
 Heat the oil in a pan on medium heat. Once hot add chopped onions. Sprinkle some salt, so onions cook faster.
 Cook till they are translucent pink and softened. Now add chopped green chili, garlic.
 Cook till all the moisture evaporates. Do stir in between. If required cover the pan.
 Now add all the spice powders (Turmeric powder, Red chili powder, Cumin powder, Coriander powder, Garam masala and Mango powder).
 Mix well and cook for a minute. Now add cooked Chickpeas along with its water. Also add cubed potatoes.
 Mix well and bring it to a boil. Simmer for 5-7 minutes or till you get the desired gravy consistency.
 Stir well and it is ready.
                
 Video Link - https://www.youtube.com/watch?v=4OjvN-firDM
                "))))
    
 
    
(defrule recipe-Peanut-Butter-Cookie
    (answer (ident peanutbutter) (text yes))
    (answer (ident sugar) (text yes))
    (answer (ident eggs) (text yes))
    =>
    (assert
        (recommendation (recipe Peanut-Butter-Cookie) (explanation " Mix all ingredients together in one bowl and place cookies on a baking sheet. Dont forget to spray! (You can also use parchment paper)
 Bake for 8-10 minutes at 350°f or until golden brown on the bottom and edges. Be careful because they will fall apart while still hot. 
 Enjoy!       
                             
 Video Link - https://www.youtube.com/watch?v=sZ7fuhui8qw
                "))))    


(defrule recipe-chocolate-cake
    (answer (ident chocolate) (text yes))
    (answer (ident butter) (text yes))
    (answer (ident sugar) (text yes))
    (answer (ident eggs) (text yes))
    =>
    (assert
        (recommendation (recipe Chocolate-Cake) (explanation " Melt chocolate add butter. Beat eggs and blends both mixes together.
 Bake at 160°c in water bath for 25 mins. 
                                   
 Video Link - https://www.youtube.com/watch?v=FxopoVLNnQg
                "))))    

;; report Module

(defmodule report)

(defrule sort-and-print
    ?r1 <- (recommendation (recipe ?f1) (explanation ?e))
    (not (recommendation (recipe ?f2&:(< (str-compare ?f2 ?f1) 0))))
    =>
    (printout t crlf " " crlf)
    (printout t " You can cook the " ?f1 " Recipe" crlf)
    (printout t " Method of Preparation: "  crlf ?e crlf)
    (retract ?r1))

;; run Module

(deffunction run-system ()
    (reset)
    (focus startup interview recommend report)
    (run))

(while TRUE
    (run-system))