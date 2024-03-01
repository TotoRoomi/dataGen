-- | raw input data such as names

module Data where
import Test.QuickCheck

-- | Creates a Gen String of names
firstname
  = ["Toto"
    ,"Harald"
    ,"Sam"
    ,"Rasmus"
    ,"Melvin"
    ,"Martin"
    ,"Anas"
    ,"Tore"
    ,"Simon"
    ]
surname
  = ["Roomi"
    ,"Olin"
    ,"Shahriari"
    ,"Craelius"
    ,"Jakobsson"
    ,"Lindefors"
    ,"Almawed"
    ,"Stenberg"
    ,"Rosén"
    ,"Maltin"
    ]
emaildomain = "@kth.se"
