-- | raw input data such as names

module Data where
import Test.QuickCheck


-- | Creates a Gen String of names
firstname
  = elements
    ["Toto"
    ,"Harald"
    ,"Sam"
    ,"Rasmus"
    ,"Melvin"
    ,"Martin"
    ,"Anas"
    ]
surname
  = elements
    ["Roomi"
    ,"Olin"
    ,"Shahriari"
    ,"Craelius"
    ,"Jakobsson"
    ,"Lindefors"
    ,"Almawed"
    ]
emaildomain = "@kth.se"
