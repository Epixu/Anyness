///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus::CTTI
{
   /// Affects CT::Executable<T>                                              
   template<class T>
   struct Executable;
}

namespace Langulus::CT
{
   /// Checks whether all decayed T are marked as executable                  
   template<class...T>
   concept Executable = Validate<Decay<T>...>
       and (LANGULUS_CTTI_CHECK(Decay<T>, Executable) and ...);

  /// Checks whether all decayed T are not marked as executable               
   template<class...T>
   concept NotExecutable = Validate<Decay<T>...>
       and ((not LANGULUS_CTTI_CHECK(Decay<T>, Executable)) and ...);
}

namespace Langulus::Flow
{
   struct Verb;
}

namespace Langulus::CTTI
{
   /// Verbs are always marked executable                                     
   template<>
   struct Executable<::Langulus::Flow::Verb> {
      static constexpr bool Enabled = true;
   };
}