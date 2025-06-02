#pragma once
#include "Types.hpp"
#include "Typenav.hpp"
#include <type_traits>


///                                                                           
///   A namespace for defining compile-time type information tags             
///                                                                           
///   Specializing <type_traits> is generally undefined behavior, but here    
/// we have alternatives that are more flexible, using type_traits as the     
/// ground truth and building concepts on top of them in Langulus::CT         
/// Read more: https://stackoverflow.com/questions/25345486                   
///   Each of the structures in this namespace correspond to a concept in     
/// Langulus::CT. These concepts can be affected in two ways (unless          
/// specified otherwise):                                                     
///   1. Specialize the appropriate CTTI::<name> struct for a type/concept    
///   2. Add a public `using CTTI_<Name> = Yes/No;` in the desired type       
///   3. Some CTTI_<Name> tags might require types or values instead -        
///      they should have additional documentation alongside them             
///                                                                           
namespace Langulus::CTTI
{

   /// Affects CT::Null<T>:                                                   
   template<class T>
   struct Null {
      static constexpr bool Enabled = ::std::is_null_pointer_v<T>;
   };
   
   /// Affects CT::Enum<T>:                                                   
   template<class T>
   struct Enum {
      static constexpr bool Enabled = ::std::is_enum_v<T>;
   };
   
   /// Affects CT::Aggregate<T>:                                              
   template<class T>
   struct Aggregate {
      static constexpr bool Enabled = ::std::is_aggregate_v<T>;
   };
   
   /// Affects CT::Fundamental<T>:                                            
   template<class T>
   struct Fundamental {
      static constexpr bool Enabled = ::std::is_fundamental_v<T>;
   };
   
   /// Affects CT::Sheddable<T>:                                              
   template<class T>
   struct Sheddable {
      static constexpr bool Enabled = false;
   };
   
   /// Can be used in two ways to satisfy CT::Typed<T>:                       
   /// 1. Specialize for T/concept having non-void Type                       
   /// 2. Add a public `using CTTI_Typed = <non void type/typelist>;` in T    
   template<class T>
   struct Typed {
      using Type = void;
   };

} // namespace Langulus::CTTI


///                                                                           
///   A namespace for defining concepts                                       
///                                                                           
/// Most of the concepts here are affected by structure specializations in    
/// the Langulus::CTTI namespace.                                             
///                                                                           
namespace Langulus::CT
{
   namespace Inner
   {
      template<class...T>
      consteval bool CheckSize() {
         static_assert(sizeof...(T) > 0, "No arguments provided");
         return true;
      }

   } // namespace Langulus::CT::Inner

   /// Check if all T are sheddable types (like intents), that serve only to  
   /// wrap data for tag dispatching and semantics. Sheddable types don't     
   /// carry any real data, and are often just a reference to the real data.  
   /// Should be aggressively optimized out of the final binary. Marking them 
   /// as sheddable means that they don't interfere with other CT checks -    
   /// these checks will act as if the sheddable type doesn't exist at all    
   /// The concept relies on CTTI::Typed for getting into the inner type      
   template<class...T>
   concept Sheddable = Inner::CheckSize<T...>() and ((CTTI::Sheddable<Deref<T>>::Enabled or LANGULUS_CTTI_DELVE_IN(T, Sheddable)) and ...);

   template<class...T>
   concept NotSheddable = Inner::CheckSize<T...>() and ((not Sheddable<Deref<T>>) and ...);

   namespace Inner
   {

      /// Extracts the inner type if T is marked as sheddable                 
      /// Otherwise results in the same type                                  
      template<CT::NotTypelist T>
      consteval CT::Typelist auto GetSheddedType() {
         using DT = Decvq<Deref<T>>;

         if constexpr (Sheddable<DT>) {
            using OuterT = typename CTTI::Typed<DT>::Type;
            if constexpr (NotVoid<OuterT>) {
               // Checked externally, T doesn't have to be complete     
               static_assert(not CT::Typelist<OuterT>,
                  "T has multiple inner types, don't know which one to use after shedding");
               return Types<OuterT> {};
            }
            else {
               // Checked internally, T has to be a complete type       
               using InnerT = typename DT::CTTI_Typed;
               static_assert(not CT::Void<InnerT>,
                  "T is CT::Sheddable, but isn't CT::Typed");
               static_assert(not CT::Typelist<InnerT>,
                  "T has multiple inner types, don't know which one to use after shedding");
               return Types<InnerT> {};
            }
         }
         else return Types<T> {};
      };

   } // namespace Langulus::CT::Inner
} // namespace Langulus::CT

namespace Langulus
{

   template<class T>
   using Shed = typename decltype(CT::Inner::GetSheddedType<T>())::First;

} // namespace Langulus

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>          
/// Used to reduce boilerplate. Will only shed references                     
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT_UNSHEDDABLE(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = Inner::CheckSize<T...>() and ((CTTI::NAME<Deref<T>>::Enabled or LANGULUS_CTTI_DELVE_IN(T, NAME)) and ...); \
      template<class...T> \
      concept Not##NAME = Inner::CheckSize<T...>() and ((not NAME<Deref<T>>) and ...); \
   }

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>          
/// It takes sheddable types into consideration. Used to reduce boilerplate   
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = Inner::CheckSize<T...>() and ((CTTI::NAME<Deref<Shed<T>>>::Enabled or LANGULUS_CTTI_DELVE_IN(Shed<T>, NAME)) and ...); \
      template<class...T> \
      concept Not##NAME = Inner::CheckSize<T...>() and ((not NAME<T>) and ...); \
   }

LANGULUS_CTTI_CONCEPT(Null);
LANGULUS_CTTI_CONCEPT(Enum);
LANGULUS_CTTI_CONCEPT(Aggregate);
LANGULUS_CTTI_CONCEPT(Fundamental);