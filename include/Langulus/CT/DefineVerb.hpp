///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Literal.hpp"


namespace Langulus
{
   /// Useful for setting CTTI_DefineVerb                                     
   template<Literal POSITIVE, Literal NEGATIVE = "", auto PRECEDENCE = 0>
   struct DefineVerb {
      static constexpr Literal Positive = POSITIVE;
      static constexpr Literal Negative = NEGATIVE;
      static constexpr float Precedence = static_cast<float>(PRECEDENCE);
      static constexpr bool Enabled = true;
   };
}

namespace Langulus::CTTI
{
   /// Can be used in three ways to satisfy CT::DefineVerb<T>:                
   /// 1. Specialize for T/concept having Enabled as true and the needed      
   ///    tokens. Negative is optional and makes the verb reversible          
   /// 2. To define a reversible verb add a public                            
   ///   `using CTTI_DefineVerb = VerbToken<"positive", "negative">;` in T    
   /// 3. To define a non-reversible verb add a public                        
   ///   `using CTTI_DefineVerb = VerbToken<"verb">;` in T                    
   template<class T>
   struct DefineVerb;

   /// Can be used in two ways to satisfy CT::DefineVerbOperator<T>:          
   /// 1. Specialize for T/concept having Enabled as true and the needed      
   ///    tokens. All operators are optional and don't affect reversibility   
   /// 2. To define a verb operator add a public                              
   ///   `using CTTI_DefineVerbOperator = VerbToken<"positive", "negative">;` 
   ///                                 or VerbToken<"positive">;`             
   ///                                 or VerbToken<"negative">;` in T        
   template<class T>
   struct DefineVerbOperator;

   /// Can be used in two ways to satisfy CT::Verbs<T>:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Verbs = <single type or Types<...>>;` in T 
   template<class T>
   struct Verbs;
}

LANGULUS_CTTI_CONCEPT(DefineVerb);
LANGULUS_CTTI_CONCEPT(DefineVerbOperator);

namespace Langulus::RTTI
{
   /// Get the name of CTTI_DefineVerb::Positive at compile-time              
   ///   @tparam T the verb to get the name of                                
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto NameOfVerb() {
      if constexpr (CT::Complete<CTTI::DefineVerb<T>>)
         return CTTI::DefineVerb<T>::Positive;
      else
         return T::CTTI_DefineVerb::Positive;
   }
   
   /// Get the name of CTTI_DefineVerb::Negative at compile-time              
   ///   @tparam T the verb to get the name of                                
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto NameOfVerbReverse() {
      if constexpr (CT::Complete<CTTI::DefineVerb<T>>)
         return CTTI::DefineVerb<T>::Negative;
      else
         return T::CTTI_DefineVerb::Negative;
   }
   
   /// Get the name of DefineVerbOperator::Positive at compile-time           
   ///   @tparam T the verb to get the name of                                
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto OperatorOfVerb() {
      if constexpr (CT::Complete<CTTI::DefineVerbOperator<T>>)
         return CTTI::DefineVerbOperator<T>::Positive;
      else
         return T::CTTI_DefineVerbOperator::Positive;
   }
   
   /// Get the name of DefineVerbOperator::Negative at compile-time           
   ///   @tparam T the verb to get the name of                                
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto OperatorOfVerbReverse() {
      if constexpr (CT::Complete<CTTI::DefineVerbOperator<T>>)
         return CTTI::DefineVerbOperator<T>::Negative;
      else
         return T::CTTI_DefineVerbOperator::Negative;
   }
}

namespace Langulus::CT::Inner
{
   /// Helper function to extract reflected verbs                             
   template<class T>
   consteval auto GetVerbs() {
      static_assert(not ::std::is_reference_v<T>,
         "Strip references first");
      static_assert(not CT::Convoluted<T>,
         "Strip constness/volatility first");

      if constexpr (Complete<CTTI::Verbs<T>>) {
         // Checked externally, T doesn't have to be complete           
         using LIST = typename CTTI::Verbs<T>::Type;
         if constexpr (CT::Typelist<LIST>)
            return LIST {};
         else
            return Types<LIST> {};
      }
      else if constexpr (requires { typename T::CTTI_Verbs; }) {
         // Checked internally, T has to be a complete type             
         using LIST = typename T::CTTI_Verbs;
         if constexpr (CT::Typelist<LIST>)
            return LIST {};
         else if constexpr (::std::same_as<LIST, No>
         or ::std::same_as<LIST, void>)
            return NoTypes {};
         else {
            static_assert(not ::std::same_as<LIST, Yes<>>,
               "Instead of Yes<> use either a verb name, "
               "or Types<multiple,verb,names> for CTTI_Verbs");
            return Types<LIST> {};
         }
      }
      else return NoTypes {};
   };
}

namespace Langulus
{
   /// Get the reflected verbs, CT::Void if none                              
   template<class T>
   using VerbsOf = decltype(CT::Inner::GetVerbs<Decvq<Deref<T>>>());
}
