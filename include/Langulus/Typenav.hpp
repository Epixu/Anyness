///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Types.hpp"
#include <concepts>


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
   /// Can be used in two ways to satisfy CT::Array<T>:                       
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Array = Yes<count>;` in T                  
   /// Optional: in many use cases, you should also make T CT::Typed          
   ///   and make sure that sizeof(T) == TypeOf<T> * ExtentOf<T>,             
   ///   if you want to reap the benefits of SIMD optimizations for T         
   template<class T>
   struct Array {
      static constexpr bool Default = true;
      static constexpr size_t Count = ::std::is_bounded_array_v<T> ? ::std::extent_v<T> : 1;
   };
   
   /// Affects CT::Sparse<T>:                                                 
   template<class T>
   struct Sparse {
      static constexpr bool Default = true;
      static constexpr bool Enabled = ::std::is_pointer_v<T>;
   };
   
   /// Affects CT::Null<T>:                                                   
   template<class T>
   struct Null {
      static constexpr bool Default = true;
      static constexpr bool Enabled = ::std::is_null_pointer_v<T>;
   };
   
   /// Affects CT::Enum<T>:                                                   
   template<class T>
   struct Enum {
      static constexpr bool Default = true;
      static constexpr bool Enabled = ::std::is_enum_v<T>;
   };
   
   /// Affects CT::Aggregate<T>:                                              
   template<class T>
   struct Aggregate {
      static constexpr bool Default = true;
      static constexpr bool Enabled = ::std::is_aggregate_v<T>;
   };
   
   /// Affects CT::Fundamental<T>:                                            
   template<class T>
   struct Fundamental {
      static constexpr bool Default = true;
      static constexpr bool Enabled = ::std::is_fundamental_v<T>;
   };
}

/// @note short-circuiting inside concepts doesn't properly work in Clang,    
///    but no one seems to care:                                              
///    https://gcc.gnu.org/bugzilla/show_bug.cgi?id=54310                     
///    This is why I've wrapped it in a lambda with 'if constexpr'            

/// Checks for reflection traits inside types themselves                      
/// Requires the TYPE to be complete in order to do that                      
#define LANGULUS_CTTI_DELVE_IN(TYPE,NAME) ([] -> bool { \
      if constexpr (::std::is_class_v<TYPE>) { \
         static_assert(::Langulus::CT::Complete<TYPE>, \
            "Can't access `CTTI_" #NAME "` inside incomplete type " #TYPE); \
         if constexpr (requires { TYPE::CTTI_##NAME::Enabled; }) \
            return TYPE::CTTI_##NAME::Enabled; \
         else return false; \
      } else return false; \
   }())

/// Checks for reflection traits outside types by CTTI struct specializations 
/// If CTTI struct is incomplete, it has no effect                            
/// If CTTI struct has a Default member, LANGULUS_CTTI_DELVE_IN is checked    
///   before utilizing the Enabled member                                     
/// If CTTI struct has no Default member, it is assumed specialized, and no   
///   LANGULUS_CTTI_DELVE_IN is required, the Enabled member is used          
#define LANGULUS_CTTI_CHECK(TYPE,NAME) ([] -> bool { \
      using ctti = ::Langulus::CTTI::NAME<TYPE>; \
      if constexpr (::Langulus::CT::Complete<ctti>) { \
         if constexpr(requires { ctti::Default; }) { \
            if constexpr (::std::is_class_v<TYPE>) { \
               static_assert(::Langulus::CT::Complete<TYPE>, \
                  "Can't access `CTTI_" #NAME "` inside incomplete type " #TYPE); \
               if constexpr(requires { TYPE::CTTI_##NAME::Enabled; }) \
                  return TYPE::CTTI_##NAME::Enabled; \
               else return ctti::Enabled; \
            } else return ctti::Enabled; \
         } else if constexpr (requires { ctti::Enabled; }) { \
            return ctti::Enabled; \
         } else return true; \
      } else return LANGULUS_CTTI_DELVE_IN(TYPE, NAME); \
   }())

namespace Langulus
{
   namespace CT
   {
      namespace Inner
      {
         /// Extracts the inner type if T is marked as sheddable              
         /// Otherwise results in an empty type list                          
         template<class T>
         consteval auto GetSheddedType() {
            using DT = ::std::remove_cvref_t<T>;

            if constexpr (Complete<DT> and ::std::is_class_v<DT>) {
               if constexpr (requires { typename DT::CTTI_Sheddable; }) {
                  using InnerT = typename DT::CTTI_Sheddable;
                  if constexpr (::std::same_as<InnerT, No>
                  or ::std::is_void_v<InnerT>)
                     return NoTypes {};
                  else {
                     static_assert(not ::std::same_as<InnerT, Yes<>>,
                        "Instead of Yes<> pick a type to shed to "
                        "for CTTI_Sheddable");
                     return Types<InnerT> {};
                  }
               }
               else return NoTypes {};
            }
            else return NoTypes {};
         };

         /// Extracts the inner type if T is marked as sheddable              
         /// If T is not sheddable, just returns T as the type                
         template<class T>
         consteval auto ShedInner() {
            using ST = decltype(GetSheddedType<T>());
            if constexpr (ST::Empty)
               return Types<T> {};
            else
               return ST {};
         };
      }
      
      /// Check if all T are sheddable types (like intents), that serve only  
      /// to wrap data for tag dispatching and semantics. Sheddable types     
      /// don't carry any real data, and often just contain a reference       
      /// to the eal data.                                                    
      /// They should be aggressively optimized out from the final binary.    
      /// Marking types as sheddable means that they don't interfere with most
      /// other CT concepts - these will act as if the sheddable type doesn't 
      /// exist at all                                                        
      ///   @attention sheddable types can only be defined through a member   
      ///      called CTTI_Sheddable, and are always assumed complete,        
      ///      otherwise this check will return false                         
      template<class...T>
      concept Sheddable = PartialValidate<T...>
          and ((not decltype(Inner::GetSheddedType<T>())::Empty) and ...);

      template<class...T>
      concept NotSheddable = PartialValidate<T...>
          and ((    decltype(Inner::GetSheddedType<T>())::Empty) and ...);

      namespace Inner
      {
         /// Extracts the bounded array size                                  
         /// Otherwise results in 1                                           
         ///   @attention assumes `CTTI::Array<T>::Default` exists for the    
         ///      unspecialized template CTTI::Array                          
         template<class T>
         consteval size_t GetBoundedArrayExtent() {
            static_assert(not ::std::is_reference_v<T>,
               "Shed all references prior to this call");
            static_assert(not CT::Sheddable<T>,
               "Shed all sheddables prior to this call");

            if constexpr (requires { CTTI::Array<T>::Default; }) {
               if constexpr (::std::is_class_v<T>) {
                  static_assert(Complete<T>,
                     "Can't access `CTTI_Array` inside incomplete type");
                  if constexpr(requires { T::CTTI_Array::Enabled; }) {
                     if constexpr (T::CTTI_Array::Enabled)
                        return T::CTTI_Array::Constant;
                     else
                        return 1;
                  }
                  else return CTTI::Array<T>::Count;
               }
               else return CTTI::Array<T>::Count;
            }
            else return CTTI::Array<T>::Count;
         };
      }
   }

   /// Sheds any sheddable types                                              
   template<class T>
   using Shed = typename decltype(CT::Inner::ShedInner<T>())::First;

   /// Get the extent of a bounded array type, or 1 if T is not an array      
   template<class T>
   constexpr size_t ExtentOf = CT::Inner::GetBoundedArrayExtent<::std::remove_reference_t<Shed<T>>>();

   /// Get the extent of an array argument, or 1 if T is not an array         
   template<class T>
   consteval size_t GetExtentOf(T&&) { return ExtentOf<::std::remove_reference_t<Shed<T>>>; }

   /// Remove a reference from type                                           
   template<class T>
   using Deref = ::std::remove_reference_t<T>;

   /// Remove a pointer from type                                             
   ///   @attention will remove references as well                            
   ///   @attention a type can still be CT::Sparse after being Deptr'ed,      
   ///      when using custom packed pointer types for example. Deptr         
   ///      removes only indirections that are part of the C++ syntax.        
   template<class T>
   using Deptr = ::std::remove_pointer_t<::std::remove_reference_t<T>>;

   /// Remove a const/volatile from a type                                    
   template<class T>
   using Decvq = ::std::remove_cv_t<T>;

   /// Remove a const from a type                                             
   template<class T>
   using Decq = ::std::remove_const_t<T>;

   /// Remove a volatile from a type                                          
   template<class T>
   using Devq = ::std::remove_volatile_t<T>;

   /// Remove an array extent from a type                                     
   ///   @attention will remove references as well                            
   template<class T>
   using Deext = ::std::remove_extent_t<::std::remove_reference_t<T>>;
   
   namespace Inner
   {
      /// Nest-strip any qualifiers, extents and indirections                 
      ///   @return a pointer to the stripped T                               
      template<class T>
      consteval auto NestedDecay() {
         using Stripped = Decvq<Deptr<Deext<T>>>;
         if constexpr (::std::same_as<T, Stripped>)
            return static_cast<Stripped*>(nullptr);
         else
            return NestedDecay<Stripped>();
      }
   }

   /// Strip a typename to its identity, removing qualifiers/pointers/etc.    
   /// This strongly guarantees, that it strips EVERYTHING, including nested  
   /// pointers and extents                                                   
   template<class T>
   using Decay = Deptr<decltype(Inner::NestedDecay<T>())>;
   
   namespace CT
   {
      /// Check if all T are bounded arrays                                   
      template<class...T>
      concept Array = PartialValidate<T...>
          and ((::std::is_bounded_array_v<Decvq<Deref<Shed<T>>>>
             or ExtentOf<Decvq<Deref<Shed<T>>>> > 1
          ) and ...);

      /// Check if all T are volatile-qualified                               
      template<class...T>
      concept Volatile = PartialValidate<T...>
          and (::std::is_volatile_v<Deref<Shed<T>>> and ...);

      /// Check if all T are sparse                                           
      ///   @attention this also includes non-pointer types that are tagged   
      ///      as custom packed pointers                                      
      template<class...T>
      concept Sparse = PartialValidate<T...>
          and (LANGULUS_CTTI_CHECK(Decvq<Deref<Shed<T>>>, Sparse) and ...);

      /// Check if all T are dense                                            
      template<class...T>
      concept Dense = PartialValidate<T...> and ((not Sparse<T>) and ...);

      /// Check if all T are constant-qualified                               
      template<class...T>
      concept Constant = PartialValidate<T...>
          and (::std::is_const_v<Deref<Shed<T>>> and ...);

      /// Check if all T are not constant-qualified                           
      template<class...T>
      concept Mutable = PartialValidate<T...>
         and ((not ::std::is_const_v<Deref<Shed<T>>>) and ...);

      /// Check if all T are either const- and/or volatile-qualified          
      template<class...T>
      concept Convoluted = PartialValidate<T...>
          and ((::std::is_const_v<Deref<Shed<T>>>
             or ::std::is_volatile_v<Deref<Shed<T>>>
          ) and ...);

      /// Check if none of T are const- and/or volatile-qualified             
      template<class...T>
      concept NotConvoluted = PartialValidate<T...>
          and (( not ::std::is_const_v<Deref<Shed<T>>>
             and not ::std::is_volatile_v<Deref<Shed<T>>>
          ) and ...);

      /// Check if all T are reference types                                  
      template<class...T>
      concept Reference = PartialValidate<T...>
          and (::std::is_reference_v<Shed<T>> and ...);

      /// Check if all T are not reference types                              
      template<class...T>
      concept NotReference = PartialValidate<T...>
          and ((not ::std::is_reference_v<Shed<T>>) and ...);

      /// Check if types have no reference/pointer/extent/qualifiers          
      ///   @attention this doesn't shed or remove references before check    
      ///   @attention a type can still be CT::Sparse while being CT::Decayed,
      ///      when using custom packed pointer types for example. Decaying   
      ///      removes only indirections that are part of the C++ syntax      
      template<class...T>
      concept Decayed = PartialValidate<T...> and ((
              not ::std::is_bounded_array_v<T>
          and not ::std::is_pointer_v<T>
          and not ::std::is_reference_v<T>
          and not ::std::is_const_v<T>
          and not ::std::is_volatile_v<T>
        ) and ...);
   
      /// Check if types have reference/pointer/extent/const/volatile         
      template<class...T>
      concept NotDecayed = PartialValidate<T...> and ((not Decayed<T>) and ...);

      /// True if T is not a pointer, has no extent with [], and isn't a      
      /// reference                                                           
      ///   @attention still allowed to be cv-qualified                       
      template<class...T>
      concept Slab = PartialValidate<T...>
          and ((not ::std::is_pointer_v<T>
            and not ::std::is_reference_v<T>
            and not ::std::is_array_v<T>
          ) and ...);
   }

   namespace Inner
   {
      /// Removes all const/volatile qualifiers from all indirections         
      /// Preserves references                                                
      ///   @return a pointer to the stripped T                               
      template<class T>
      consteval CT::Typelist auto NestedDecvq() {
         if constexpr (::std::is_rvalue_reference_v<T>)
            return Types<typename decltype(NestedDecvq<::std::remove_reference_t<T>>())::First&&> {};
         else if constexpr (::std::is_lvalue_reference_v<T>)
            return Types<typename decltype(NestedDecvq<::std::remove_reference_t<T>>())::First&> {};
         else if constexpr (::std::is_pointer_v<T>)
            return Types<typename decltype(NestedDecvq<::std::remove_pointer_t<T>>())::First*> {};
         else if constexpr (::std::is_bounded_array_v<T>)
            return Types<typename decltype(NestedDecvq<::std::remove_extent_t<T>>())::First [::std::extent_v<T>]> {};
         else
            return Types<::std::remove_cv_t<T>> {};
      }

      /// Count the number of indirections                                    
      ///   @return the number of pointers in a type                          
      template<class T>
      consteval size_t CountIndirections() {
         if constexpr (::std::is_pointer_v<T>)
            return 1 + CountIndirections<Deref<Shed<Deptr<T>>>>();
         else
            return 0;
      }
   }

   /// Strip all qualifiers on all levels of indirection of a type            
   /// const volatile void * const * const becomes void**                     
   /// This strongly guarantees, that it strips EVERYTHING, including nested  
   /// pointer/array constness/volatileness, etc.                             
   template<class T>
   using DecvqAll = typename decltype(Inner::NestedDecvq<T>())::First;

   /// Count the number of indirections                                       
   ///   @attention this considers only C+++ syntax pointers, not custom      
   ///      pointer types                                                     
   ///   @attention sparse sheddables will contribute to the count            
   template<class T>
   static constexpr size_t IndirectsOf = Inner::CountIndirections<Deref<Shed<T>>>();
}


/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>          
/// Used to reduce boilerplate                                                
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention will only shed references                                    
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT_UNSHEDDABLE(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = PartialValidate<T...> \
          and (LANGULUS_CTTI_CHECK(Deref<T>, NAME) and ...); \
      template<class...T> \
      concept Not##NAME = PartialValidate<T...> \
          and ((not LANGULUS_CTTI_CHECK(Deref<T>, NAME)) and ...); \
   }

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>          
/// Used to reduce boilerplate                                                
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention will shed only references and cv qualifiers                  
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT_UNSHEDDABLE_DECVQ(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = PartialValidate<T...> \
          and (LANGULUS_CTTI_CHECK(Decvq<Deref<T>>, NAME) and ...); \
      template<class...T> \
      concept Not##NAME = PartialValidate<T...> \
          and ((not LANGULUS_CTTI_CHECK(Decvq<Deref<T>>, NAME)) and ...); \
   }

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>          
/// Used to reduce boilerplate                                                
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention will shed all sheddables, as well as references after that   
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = PartialValidate<T...> \
          and (LANGULUS_CTTI_CHECK(Deref<Shed<T>>, NAME) and ...); \
      template<class...T> \
      concept Not##NAME = PartialValidate<T...> \
          and ((not LANGULUS_CTTI_CHECK(Deref<Shed<T>>, NAME)) and ...); \
   }

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>          
/// Used to reduce boilerplate                                                
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention will shed all sheddables, as well as references and cv       
///      qualifiers after that                                                
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT_DECVQ(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = PartialValidate<T...> \
          and (LANGULUS_CTTI_CHECK(Decvq<Deref<Shed<T>>>, NAME) and ...); \
      template<class...T> \
      concept Not##NAME = PartialValidate<T...> \
          and ((not LANGULUS_CTTI_CHECK(Decvq<Deref<Shed<T>>>, NAME)) and ...); \
   }

LANGULUS_CTTI_CONCEPT(Null);
LANGULUS_CTTI_CONCEPT(Enum);
LANGULUS_CTTI_CONCEPT(Aggregate);
LANGULUS_CTTI_CONCEPT(Fundamental);
