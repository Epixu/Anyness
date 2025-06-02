///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../CTTI.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Exception<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Exception = Yes/No;` in T                  
   template<class T>
   struct Exception {
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT_UNSHEDDABLE(Exception);

namespace Langulus
{

   ///                                                                        
   ///   General exception                                                    
   ///                                                                        
   /// It is an equivalent to std::runtime_error, but with additional info    
   /// for debug builds, like message and location strings                    
   ///                                                                        
   struct Exception {
      using CTTI_Exception = Yes;

      static constexpr const char* DefaultMessage  = "<no information provided>";
      static constexpr const char* DefaultLocation = "<unknown location>";

      #if LANGULUS(DEBUG)
         // Exception message                                           
         const char* mMessage  = DefaultMessage;
         // Exception location, as a separate literal to avoid concat   
         const char* mLocation = DefaultLocation;
      #endif
   };

} // namespace Langulus
