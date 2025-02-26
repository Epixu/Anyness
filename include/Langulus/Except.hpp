///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once


namespace Langulus
{

   ///                                                                        
   ///   General exception                                                    
   ///                                                                        
   /// It is an equivalent to std::runtime_error, but with additional info    
   /// for debug builds, like message and location strings                    
   ///                                                                        
   struct Exception {
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