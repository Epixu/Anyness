///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"
#include "../Assume.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Referenced<T>:                  
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Referenced = Yes;` in T                    
   template<class T>
   struct Referenced {
      static constexpr bool Enabled = false;
   };
}

LANGULUS_CTTI_CONCEPT(Referenced);

namespace Langulus
{
   ///                                                                        
   /// Base types off this one, to make them CT::Referenced and provide the   
   /// required interface for it                                              
   ///                                                                        
   class Referenced {
      int mReferences = 1;

   public:
      using CTTI_Referenced = Yes<>;

      LANGULUS(INLINED)
      ~Referenced() {
         AssumeDev(mReferences <= 1,
            "Leftover references (", mReferences,") on instance destruction. "
            "When inheriting from Referenced, you're supposed to "
            "implement either an appropriate destructor (or surrounding logic) "
            "that makes sure references are reduced down to zero, before "
            "this destructor gets called. This is necessary to make sure "
            "that no leaks happen."
         );
         AssumeDevWarn(mReferences != 1, HERE(),
            "Referenced object destroyed before last "
            "reference was removed - was it on the stack?"
         );
      }

      LANGULUS(ALWAYS_INLINED)
      int GetReferences() const noexcept {
         return mReferences;
      }

      LANGULUS(ALWAYS_INLINED)
      int Reference(int x) has_assumptions {
         AssumeDev(mReferences or x == 0, "Dead instance resurrection/overkill");
         AssumeDev(x >= 0 or mReferences >= -x, "Live instance overkill");
         mReferences += x;
         return mReferences;
      }
   };
}
