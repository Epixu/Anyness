///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Integer.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Index<T>:                       
   /// All integer types are considered indices by default                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Index = Yes<>;` in T                       
   template<class T>
   struct Index {
      static constexpr bool Default = true;
      static constexpr bool Enabled = CT::Integer<T>;
   };
}

//LANGULUS_CTTI_CONCEPT_DECVQ(Index);
LANGULUS_CTTI_CONCEPT_UNSHEDDABLE(Index);

namespace Langulus::Index
{
   namespace Inner
   {
      template<bool VALID>
      struct Common {
         using CTTI_Index = Yes<>;

         constexpr explicit operator bool() const noexcept {
            return VALID;
         }
      };

      struct All      : Common<true>  {};
      struct Many     : Common<true>  {};
      struct Single   : Common<true>  {};
      struct None     : Common<false> {};
      struct Front    : Common<true>  {};
      struct Middle   : Common<true>  {};
      struct Back     : Common<true>  {};
      struct Mode     : Common<true>  {};
      struct Biggest  : Common<true>  {};
      struct Smallest : Common<true>  {};
      struct Random   : Common<true>  {};
      struct First    : Common<true>  {};
      struct Last     : Common<true>  {};
   }

   /// Equivalent to the container's initialized count                        
   constexpr auto All = Inner::All {};

   /// Equivalent to a count larger than one                                  
   constexpr auto Many = Inner::Many {};

   /// Equivalent to a count of one                                           
   constexpr auto Single = Inner::Single {};

   /// Equivalent to a count of zero, or Container::CountType's max value     
   /// For std containers it's equivalent to 'npos'                           
   constexpr auto None = Inner::None {};

   /// Equivalent to the space before the first element in continuous memory  
   constexpr auto Front = Inner::Front {};

   /// Equivalent to the middle element of continuous memory, or the space    
   /// between the two middle elements, in the case that count is even        
   constexpr auto Middle = Inner::Middle {};

   /// Equivalent to the space after the last element in continuous memory    
   /// i.e. one-past the last element                                         
   constexpr auto Back = Inner::Back {};

   /// Equivalent to the first index of the most occuring element in memory   
   constexpr auto Mode = Inner::Mode {};

   /// Equivalent to the index of the first largest element in memory         
   constexpr auto Biggest = Inner::Biggest {};

   /// Equivalent to the index of the first smallest element in memory        
   constexpr auto Smallest = Inner::Smallest {};

   /// A random index inside a valid memory range                             
   constexpr auto Random = Inner::Random {};

   /// Equivalent to 0 for continuous memory                                  
   constexpr auto First = Inner::First {};

   /// Equivalent to size-1 for continuous memory                             
   constexpr auto Last = Inner::Last {};

   /// An arithmetic index that is evaluated to false if equal to npos        
   /// in order to easily use it in 'if-statements' instead of constantly     
   /// comparing against npos like a chump.                                   
   template<class T>
   struct At {
      using CTTI_Index = Yes<>;

      T index;

      constexpr explicit operator bool() const noexcept {
         return index != ::std::numeric_limits<T>::max();
      }
   };

   template<class T> At(T&&) -> At<T>;
}
