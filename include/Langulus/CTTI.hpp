#pragma once
#include "Types.hpp"
#include "Typenav.hpp"
#include <type_traits>


/// A namespace for defining compile-time type information tags               
/// Specializing <type_traits> is generally undefined behavior, but here      
/// we have alternatives that are more flexible, using type_traits as the     
/// ground truth and building concepts on top of them, in Langulus::CT        
/// Read more: https://stackoverflow.com/questions/25345486                   
namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Null<T>:                        
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Null = Yes/No;` in T                       
   template<class T>
   struct Null {
      static constexpr bool Enabled = ::std::is_null_pointer_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Enum<T>:                        
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Enum = Yes/No;` in T                       
   template<class T>
   struct Enum {
      static constexpr bool Enabled = ::std::is_enum_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Aggregate<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Aggregate = Yes/No;` in T                  
   template<class T>
   struct Aggregate {
      static constexpr bool Enabled = ::std::is_aggregate_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Fundamental<T>:                 
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Fundamental = Yes/No;` in T                
   template<class T>
   struct Fundamental {
      static constexpr bool Enabled = ::std::is_fundamental_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Sheddable<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Sheddable = Yes/No;` in T                  
   template<class T>
   struct Sheddable {
      static constexpr bool Enabled = false;
   };
   
   /// Can be used in two ways to satisfy CT::Typed<T>:                       
   /// 1. Specialize for T/concept having non-void Type                       
   /// 2. Add a public `using CTTI_Typed = <non void type>;` in T             
   template<class T>
   struct Typed {
      using Type = void;
   };

} // namespace Langulus::CTTI

namespace Langulus::CT
{

   /// Check if all T are sheddable types (like intents), that serve only to  
   /// wrap data for tag dispatching and semantics. Sheddable types don't     
   /// carry any real data, often just a reference to the real data, and      
   /// should be aggressively optimized out of the final binary. Marking them 
   /// as sheddable means that they don't interfere with other CT checks -    
   /// these checks will act as if the sheddable type doesn't exist at all    
   /// The concept relies on CTTI::Typed for getting into the inner type      
   template<class...T>
   concept Sheddable = ((CTTI::Sheddable<Deref<T>>::Enabled or (not ::std::is_pointer_v<T> and Decay<T>::CTTI_Sheddable::Enabled)) and ...);

   template<class...T>
   concept NotSheddable = ((not Sheddable<Deref<T>>) and ...);

   namespace Inner
   {

      template<class T>
      consteval CT::Typelist auto GetSheddedType() {
         if constexpr (Sheddable<T>) {
            if constexpr (NotVoid<typename CTTI::Typed<T>::Type>) {
               // Checked externally, T doesn't have to be complete     
               return Types<typename CTTI::Typed<T>::Type> {};
            }
            else if constexpr (requires { typename T::CTTI_Typed; }) {
               // Checked internally, T has to be a complete type       
               return Types<typename T::CTTI_Typed> {};
            }
            else static_assert(false, "Type is marked as Sheddable, but isn't marked as Typed");
         }
         else return Types<T> {};
      };

   } // namespace Langulus::CT::Inner

   template<class T>
   using Shed = typename decltype(Inner::GetSheddedType<Deref<T>>())::First;

} // namespace Langulus::CT


/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>          
/// Used to reduce boilerplate                                                
#define LANGULUS_CTTI_CONCEPT_UNSHEDDABLE(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = ((CTTI::NAME<Deref<T>>::Enabled or (not ::std::is_pointer_v<T> and Decay<T>::CTTI_##NAME::Enabled)) and ...); \
      template<class...T> \
      concept Not##NAME = ((not NAME<Deref<T>>) and ...); \
   }

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>          
/// It takes sheddable types into consideration. Used to reduce boilerplate   
#define LANGULUS_CTTI_CONCEPT(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = ((CTTI::NAME<Deref<Shed<T>>>::Enabled or (not ::std::is_pointer_v<Shed<T>> and Decay<Shed<T>>::CTTI_##NAME::Enabled)) and ...); \
      template<class...T> \
      concept Not##NAME = ((not NAME<T>) and ...); \
   }

LANGULUS_CTTI_CONCEPT(Null);
LANGULUS_CTTI_CONCEPT(Enum);
LANGULUS_CTTI_CONCEPT(Aggregate);
LANGULUS_CTTI_CONCEPT(Fundamental);

namespace Langulus::CT
{

   /// Check if all T are function signatures                                 
   ///   @attention std::function, lambdas, classes with overloaded           
   ///      operator() and pointers to functions don't count as function      
   ///      signatures - use Decay<T> to get the underlying signature         
   template<class...T>
   concept Function = (::std::is_function_v<T> and ...);

} // namespace Langulus::CT