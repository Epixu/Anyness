#pragma once
#include "../Literal.hpp"


namespace Langulus
{
   /// Useful for setting CTTI_DefineVerb                                     
   template<Literal POSITIVE, Literal NEGATIVE = "", auto PRECEDENCE = 0>
   struct VerbToken {
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
   struct DefineVerb {
      static constexpr Literal Positive = "<not a verb>";
      static constexpr Literal Negative = "<not a verb>";
      static constexpr float Precedence = 0;
      static constexpr bool Enabled = false;
   };

   /// Can be used in two ways to satisfy CT::DefineVerbOperator<T>:          
   /// 1. Specialize for T/concept having Enabled as true and the needed      
   ///    tokens. All operators are optional and don't affect reversibility   
   /// 2. To define a verb operator add a public                              
   ///   `using CTTI_DefineVerbOperator = VerbToken<"positive", "negative">;` 
   ///                                 or VerbToken<"positive">;`             
   ///                                 or VerbToken<"negative">;` in T        
   template<class T>
   struct DefineVerbOperator {
      static constexpr Literal Positive = "<not a verb>";
      static constexpr Literal Negative = "<not a verb>";
      static constexpr bool Enabled = false;
   };

   /// Can be used in two ways to satisfy CT::Verbs<T>:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Verbs = <single type or Types<...>>;` in T 
   template<class T>
   struct Verbs {
      using Type = void;
      static constexpr bool Enabled = false;
   };
}

LANGULUS_CTTI_CONCEPT(DefineVerb);
LANGULUS_CTTI_CONCEPT(DefineVerbOperator);

namespace Langulus::RTTI
{
   /// Get the name of CTTI_DefineVerb::Positive at compile-time              
   ///   @tparam T - the verb to get the name of                              
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto NameOfVerb() {
      if constexpr (CTTI::DefineVerb<T>::Enabled)
         return CTTI::DefineVerb<T>::Positive;
      else
         return T::CTTI_DefineVerb::Positive;
   }
   
   /// Get the name of CTTI_DefineVerb::Negative at compile-time              
   ///   @tparam T - the verb to get the name of                              
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto NameOfVerbReverse() {
      if constexpr (CTTI::DefineVerb<T>::Enabled)
         return CTTI::DefineVerb<T>::Negative;
      else
         return T::CTTI_DefineVerb::Negative;
   }
   
   /// Get the name of DefineVerbOperator::Positive at compile-time           
   ///   @tparam T - the verb to get the name of                              
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto OperatorOfVerb() {
      if constexpr (CTTI::DefineVerbOperator<T>::Enabled)
         return CTTI::DefineVerbOperator<T>::Positive;
      else if constexpr (requires { T::CTTI_DefineVerbOperator::Enabled; })
         return T::CTTI_DefineVerbOperator::Positive;
      else
         return Literal {""};
   }
   
   /// Get the name of DefineVerbOperator::Negative at compile-time           
   ///   @tparam T - the verb to get the name of                              
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto OperatorOfVerbReverse() {
      if constexpr (CTTI::DefineVerbOperator<T>::Enabled)
         return CTTI::DefineVerbOperator<T>::Negative;
      else if constexpr (requires { T::CTTI_DefineVerbOperator::Enabled; })
         return T::CTTI_DefineVerbOperator::Negative;
      else
         return Literal {""};
   }
}

namespace Langulus::CT
{
   namespace Inner
   {
      /// Helper function to extract reflected verbs                          
      template<class T>
      consteval CT::Typelist auto GetVerbs() {
         static_assert(not ::std::is_reference_v<T>,
            "Strip references first");
         static_assert(not CT::Convoluted<T>,
            "Strip constness/volatility first");

         if constexpr (CTTI::Verbs<T>::Enabled) {
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
            else
               return Types<LIST> {};
         }
         else return Types<void> {};
      };
   }
}

namespace Langulus
{
   /// Get the reflected verbs, CT::Void if none                              
   template<class T>
   using VerbsOf = decltype(CT::Inner::GetVerbs<Decvq<Deref<T>>>());
}
