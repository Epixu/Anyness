///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Utils/Types.hpp"
#include <concepts>


///                                                                           
///   A namespace for defining compile-time type information tags.            
///                                                                           
///   Specializing <type_traits> is generally undefined behavior, but here    
/// we have alternatives that are more flexible, using type_traits as the     
/// ground truth and building concepts on top of them in Langulus::CT.        
/// Read more: https://stackoverflow.com/questions/25345486                   
///   Each of the structures in this namespace correspond to a concept in     
/// Langulus::CT. These concepts can be affected in two ways (unless          
/// specified otherwise):                                                     
///   1. Specialize the appropriate CTTI::<name> struct for a type/concept    
///   2. Add a public `using CTTI_<Name> = Yes/No;` in the desired type       
///   3. Some CTTI_<Name> tags might require types or values instead -        
///      they should have additional documentation alongside them             
namespace Langulus::CTTI
{
   /// MARK: CTTI                                                             
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

/// Checks for reflection traits inside types themselves.                     
/// Requires the TYPE to be complete in order to do that.                     
#define LANGULUS_CTTI_DELVE_IN(TYPE,NAME,FALLBACK) ([] consteval -> bool { \
      if constexpr (::std::is_class_v<TYPE>) { \
         static_assert(::Langulus::CT::Complete<TYPE>, \
            "Can't access `CTTI_" #NAME "` inside incomplete type " #TYPE); \
         if constexpr (not ::Langulus::CT::Complete<TYPE>) \
            return FALLBACK; \
         else if constexpr (requires { TYPE::CTTI_##NAME::Enabled; }) \
            return TYPE::CTTI_##NAME::Enabled; \
         else return FALLBACK; \
      } else return FALLBACK; \
   }())

/// Checks for reflection traits outside types by CTTI struct specializations 
/// If CTTI struct is incomplete, it has no effect.                           
/// If CTTI struct has a Default member, LANGULUS_CTTI_DELVE_IN is checked    
///   before utilizing the Enabled member.                                    
/// If CTTI struct has no Default member, it is assumed specialized, and no   
///   LANGULUS_CTTI_DELVE_IN is required, the Enabled member is used.         
#define LANGULUS_CTTI_CHECK(TYPE,NAME) ([] consteval -> bool { \
      using ctti = ::Langulus::CTTI::NAME<TYPE>; \
      if constexpr (::Langulus::CT::Complete<ctti>) { \
         if constexpr(requires { ctti::Default; }) { \
            return LANGULUS_CTTI_DELVE_IN(TYPE, NAME, ctti::Enabled); \
         } else if constexpr (requires { ctti::Enabled; }) { \
            return ctti::Enabled; \
         } else return true; \
      } else return LANGULUS_CTTI_DELVE_IN(TYPE, NAME, false); \
   }())

namespace Langulus
{
   /// Remove a reference from type                                           
   template<class T>
   using Deref = ::std::remove_reference_t<T>;

   /// Remove a const/volatile from a type                                    
   template<class T>
   using Decvq = ::std::remove_cv_t<T>;

   /// Remove a const from a type                                             
   template<class T>
   using Decq = ::std::remove_const_t<T>;

   /// Remove a volatile from a type                                          
   template<class T>
   using Devq = ::std::remove_volatile_t<T>;

   /// Remove the topmost array extent from a type                            
   ///   @attention will remove references as well                            
   template<class T>
   using Deext = ::std::remove_extent_t<Deref<T>>; //TODO Deext-ing a custom CT::Array should give the inner type!!!

   namespace CT
   {
      namespace Inner
      {
         /// Extracts the inner type if T is marked as sheddable.             
         /// Otherwise results in an empty type list.                         
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

         /// Extracts the inner type if T is marked as sheddable.             
         /// If T is not sheddable, just returns T as the type.               
         ///   @attention strips _all_ sheddables                             
         template<class T>
         consteval auto ShedInner() {
            using ST = decltype(GetSheddedType<T>());
            if constexpr (ST::Empty)
               return Types<T> {};
            else
               return ShedInner<typename ST::First>();
         };
      }
      
      /// Check if all T are sheddable types (like intents), that serve only  
      /// to wrap data for tag dispatching and semantics. Sheddable types     
      /// don't carry any real data, and often just contain a reference       
      /// to the real data.                                                   
      /// They should be aggressively optimized out from the final binary.    
      /// Marking types as sheddable means that they don't interfere with most
      /// other CT concepts - these will act as if sheddable types don't      
      /// exist at all, with the exception of CT::Sheddable itself.           
      ///   @attention sheddable types can only be defined through a member   
      ///      called CTTI_Sheddable, and are always assumed complete,        
      ///      otherwise this check will return false                         
      template<class...T>
      concept Sheddable = PartialValidate<T...>
          and ((not decltype(Inner::GetSheddedType<T>())::Empty) and ...);

      template<class...T>
      concept NotSheddable = PartialValidate<T...>
          and ((not Sheddable<T>) and ...);
      
      namespace Inner
      {
         /// Extracts the bounded array size. Otherwise results in 1.         
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

         /// Multiplies all the nested bounded arrays' size together.         
         /// Results in 1 if T is not an array.                               
         template<class T>
         consteval size_t GetBoundedArrayExtentNested() {
            constexpr size_t result = GetBoundedArrayExtent<T>();
            if constexpr (not ::std::same_as<T, Deext<T>>)
               return result * GetBoundedArrayExtentNested<Deext<T>>();
            else
               return result;
         };

         /// Removes a pointer from the type. Supports custom pointers.       
         ///   @attention if an incomplete type is reached the nesting ceases 
         template<class T, uint TIMES>
         consteval auto NestedDeptr() {
            static_assert(not ::std::is_reference_v<T>,
               "Shed all references prior to this call");
            static_assert(TIMES >= 1,
               "Can't deptr zero times");

            if constexpr (not Complete<T>)
               return Types<T> {};
            else {
               if constexpr (::std::is_pointer_v<T>) {
                  if constexpr (::std::is_void_v<::std::remove_pointer_t<T>>)
                     return NoTypes {};
                  else {
                     // Conventional pointer dereferencing              
                     using deptr_once = ::std::remove_pointer_t<T>;
                     if constexpr (TIMES == 1)
                        return Types<deptr_once> {};
                     else
                        return NestedDeptr<deptr_once, TIMES - 1>();
                  }
               }
               else if constexpr (::std::is_bounded_array_v<T>) {
                  // Conventional bounded array dereferencing           
                  using deptr_once = ::std::remove_extent_t<T>;
                  if constexpr (TIMES == 1)
                     return Types<deptr_once> {};
                  else
                     return NestedDeptr<deptr_once, TIMES - 1>();
               }
               else if constexpr (LANGULUS_CTTI_CHECK(T, Sparse)) {
                  // Custom pointer dereferencing                       
                  static_assert(requires(T t) { *t; },
                     "Custom pointer doesn't have unary operator*");
                  
                  using deptr_once = Deref<decltype(*LglsFake(T))>;
                  if constexpr (TIMES == 1)
                     return Types<deptr_once> {};
                  else
                     return NestedDeptr<deptr_once, TIMES - 1>();
               }
               else return Types<T> {};
            }
         }
      }
   }

   /// Sheds any sheddable types                                              
   template<class T>
   using Shed = typename decltype(CT::Inner::ShedInner<T>())::First;
   template<class T>
   using ShedDeref = Deref<Shed<T>>;

   /// Get the extent of a bounded array type, or 1 if T is not an array      
   template<class T>
   constexpr size_t ExtentOf = CT::Inner::GetBoundedArrayExtent<ShedDeref<T>>();

   /// Get the extent of an array argument, or 1 if T is not an array         
   template<class T>
   constexpr size_t GetExtentOf(T&&) { return ExtentOf<ShedDeref<T>>; }

   /// Get all nested extents of a bounded array type, multiplied, or 1       
   /// if T is not an array                                                   
   template<class T>
   constexpr size_t AllExtentsOf = CT::Inner::GetBoundedArrayExtentNested<ShedDeref<T>>();

   /// Get all nested extents of a bounded array argument, multiplied, or 1   
   /// if T is not an array                                                   
   template<class T>
   constexpr size_t GetAllExtentsOf(T&&) { return AllExtentsOf<ShedDeref<T>>; }

   /// Remove a number of pointers from type. Supports custom pointer types.  
   ///   @attention may result in a reference                                 
   ///   @attention if an incomplete type is reached the nesting ceases       
   template<class T, uint TIMES = 1>
   using Deptr = typename decltype(CT::Inner::NestedDeptr<ShedDeref<T>, TIMES>())::First;

   namespace Inner
   {
      /// Nest-strip any qualifiers, extents, references, sheddables, and     
      /// indirections (including custom pointers).                           
      ///   @return a pointer to the stripped T                               
      ///   @attention if an incomplete type is reached, the nesting ceases   
      template<class T>
      consteval auto NestedDecay() {
         using Stripped = Decvq<Deref<Deptr<T>>>;
         if constexpr (::std::same_as<T, Stripped>)
            return static_cast<Stripped*>(nullptr);
         else
            return NestedDecay<Stripped>();
      }
   }

   /// Strip a typename to its identity, removing qualifiers, indirections    
   /// (even custom ones), references, and sheddables. This strongly          
   /// guarantees, that it strips EVERYTHING, including nested pointers,      
   /// sheddables, and extents.                                               
   template<class T>
   using Decay = ::std::remove_pointer_t<decltype(Inner::NestedDecay<T>())>;
   
   /// MARK: CT                                                               
   namespace CT
   {
      /// Check if all T are bounded arrays                                   
      template<class...T>
      concept Array = PartialValidate<T...>
          and ((::std::is_bounded_array_v<ShedDeref<T>>
             or (ExtentOf<ShedDeref<T>>) > 1
          ) and ...);

      /// Check if all T are volatile-qualified                               
      template<class...T>
      concept Volatile = PartialValidate<T...>
          and (::std::is_volatile_v<ShedDeref<T>> and ...);

      /// Check if all T are sparse. Supports custom pointer types.           
      template<class...T>
      concept Sparse = PartialValidate<T...>
          and (LANGULUS_CTTI_CHECK(Decvq<ShedDeref<T>>, Sparse) and ...);

      /// Check if all T are custom pointer types.                            
      template<class...T>
      concept CustomPointer = PartialValidate<T...> and Sparse<T...>
          and ((not ::std::is_pointer_v<ShedDeref<T>>) and ...);

      /// Check if all T are dense. Detects custom pointer types.             
      template<class...T>
      concept Dense = PartialValidate<T...> and ((not Sparse<T>) and ...);

      /// Check if all T are constant-qualified                               
      template<class...T>
      concept Constant = PartialValidate<T...>
          and (::std::is_const_v<ShedDeref<T>> and ...);

      /// Check if all T are not constant-qualified                           
      template<class...T>
      concept Mutable = PartialValidate<T...>
         and ((not Constant<T>) and ...);

      /// Check if all T are either const- and/or volatile-qualified          
      template<class...T>
      concept Convoluted = PartialValidate<T...>
          and ((::std::is_const_v<ShedDeref<T>>
             or ::std::is_volatile_v<ShedDeref<T>>
          ) and ...);

      /// Check if none of T are const- and/or volatile-qualified             
      template<class...T>
      concept NotConvoluted = PartialValidate<T...>
          and ((not Convoluted<T>) and ...);

      /// Check if all T are reference types                                  
      template<class...T>
      concept Reference = PartialValidate<T...>
          and (::std::is_reference_v<Shed<T>> and ...);

      /// Check if all T are not reference types                              
      template<class...T>
      concept NotReference = PartialValidate<T...>
          and ((not ::std::is_reference_v<Shed<T>>) and ...);

      /// Check if all types have no reference/pointer/extent/qualifiers.     
      /// Includes support for custom pointers.                               
      ///   @attention this doesn't shed or remove references before check    
      template<class...T>
      concept Decayed = PartialValidate<T...> and LglsSif(((
            ::std::is_bounded_array_v<T>
         or ::std::is_reference_v<T>
         or ::std::is_const_v<T>
         or ::std::is_volatile_v<T>) or ...),
            return false,
            return CT::Dense<T...>
         );
   
      /// Check if types have reference/pointer/extent/const/volatile         
      ///   @attention this doesn't shed or remove references before check    
      template<class...T>
      concept NotDecayed = PartialValidate<T...> and ((not Decayed<T>) and ...);

      /// True if T is not a pointer (even a custom one), has no extent       
      /// with [] and isn't a reference.                                      
      ///   @attention still allowed to be cv-qualified                       
      template<class...T>
      concept Slab = PartialValidate<T...> and LglsSif(((
            ::std::is_reference_v<T>
         or ::std::is_array_v<T>) or ...),
            return false,
            return CT::Dense<T...>
         );
         
      namespace Inner
      {
         /// Checks for const/volatile qualifiers in all indirections/refs.   
         template<class T>
         consteval bool NestedCheckCVQ() {
            if constexpr (CT::Convoluted<T>)
               return true;
            else if constexpr (::std::is_reference_v<T>)
               return NestedCheckCVQ<Deref<T>>();
            else if constexpr (CT::Sparse<T>)
               return NestedCheckCVQ<Deptr<T>>();
            else if constexpr (::std::is_bounded_array_v<T>)
               return NestedCheckCVQ<::std::remove_extent_t<T>>();
            else
               return false;
         }

         /// Checks if all indirections/refs are constant.                    
         template<class T>
         consteval bool NestedConstantEverywhere() {
            if constexpr (::std::is_reference_v<T>)
               return CT::Constant<Deref<T>> and NestedConstantEverywhere<Deref<T>>();
            else if constexpr (CT::Sparse<T>)
               return CT::Constant<Deptr<T>> and NestedConstantEverywhere<Deptr<T>>();
            else if constexpr (::std::is_bounded_array_v<T>)
               return CT::Constant<Deext<T>> and NestedConstantEverywhere<Deext<T>>();
            else
               return CT::Constant<T>;
         }
      }
      
      /// Check if all T are either const- and/or volatile-qualified on any   
      /// level of indirection.                                               
      template<class...T>
      concept ConvolutedAnywhere = PartialValidate<T...>
          and (Inner::NestedCheckCVQ<T>() and ...);

      /// Check if none of T are const- and/or volatile-qualified on any      
      /// level of indirection.                                               
      template<class...T>
      concept NotConvolutedAnywhere = PartialValidate<T...>
          and ((not ConvolutedAnywhere<T>) and ...);

      /// Check if all T are either const- and/or volatile-qualified on any   
      /// level of indirection.                                               
      template<class...T>
      concept ConstantEverywhere = PartialValidate<T...>
          and (Inner::NestedConstantEverywhere<T>() and ...);

      /// Check if none of T are const- and/or volatile-qualified on any      
      /// level of indirection.                                               
      template<class...T>
      concept NotConstantEverywhere = PartialValidate<T...>
          and ((not ConstantEverywhere<T>) and ...);
   }

   ///                                                                        
   /// Structure for describing custom packed pointers.                       
   /// The default PointerSpecification with all members initialized to zero  
   /// corresponds to a pointer with sizeof(void*) and thus not packed.       
   struct PointerSpecification {
      uint PoolBits = 0;
      uint EntryBits = 0;
      uint OffsetBits = 0;

      constexpr uint GetTotalBits() const noexcept {
         const auto total = PoolBits + EntryBits + OffsetBits;
         return total ? total : sizeof(void*)*8;
      }
      
      constexpr uint GetTotalBytes() const noexcept {
         const auto total = PoolBits + EntryBits + OffsetBits;
         return total ? total/8u : sizeof(void*);
      }
      
      constexpr bool IsPacked() const noexcept {
         return (PoolBits + EntryBits + OffsetBits) != 0;
      }
   };
   
   namespace Inner
   {
      /// Removes all extents from a bounded array.                           
      /// Removes references.                                                 
      template<class T>
      consteval CT::Typelist auto NestedDeext() {
         if constexpr (CT::Array<T>)
            return Types<typename decltype(NestedDeext<Deext<T>>())::First> {};
         else
            return Types<T> {};
      }

      /// Removes all const/volatile qualifiers from all indirections.        
      /// Supports custom pointers. Preserves references.                     
      template<class T>
      consteval CT::Typelist auto NestedDecvq() {
         if constexpr (::std::is_rvalue_reference_v<T>)
            return Types<typename decltype(NestedDecvq<Deref<T>>())::First&&> {};
         else if constexpr (::std::is_lvalue_reference_v<T>)
            return Types<typename decltype(NestedDecvq<Deref<T>>())::First&> {};
         else if constexpr (::std::is_pointer_v<T>)
            return Types<typename decltype(NestedDecvq<::std::remove_pointer_t<T>>())::First*> {};
         else if constexpr (::std::is_bounded_array_v<T>)
            return Types<typename decltype(NestedDecvq<::std::remove_extent_t<T>>())::First [::std::extent_v<T>]> {};
         else if constexpr (CT::Complete<T>) {
            if constexpr (CT::CustomPointer<T>)
               return Types<typename T::MakeDecvqAll> {};
            else
               return Types<::std::remove_cv_t<T>> {};
         }
         else return Types<::std::remove_cv_t<T>> {};
      }

      /// Adds const qualifier to all levels of indirection except the top.   
      /// Supports custom pointers. Preserves references.                     
      template<class T>
      consteval CT::Typelist auto NestedConst() {
         if constexpr (::std::is_rvalue_reference_v<T>)
            return Types<typename decltype(NestedConst<Deref<T>>())::First const&&> {};
         else if constexpr (::std::is_lvalue_reference_v<T>)
            return Types<typename decltype(NestedConst<Deref<T>>())::First const&> {};
         else if constexpr (::std::is_pointer_v<T>)
            return Types<typename decltype(NestedConst<::std::remove_pointer_t<T>>())::First const*> {};
         else if constexpr (::std::is_bounded_array_v<T>)
            return Types<typename decltype(NestedConst<::std::remove_extent_t<T>>())::First const [::std::extent_v<T>]> {};
         else if constexpr (CT::Complete<T>) {
            if constexpr (CT::CustomPointer<T>)
               return Types<typename T::MakeConstAll> {};
            else
               return Types<T> {};
         }
         else return Types<T> {};
      }

      /// Count the number of indirections, including custom pointers.        
      ///   @return the number of pointers in a type                          
      template<class T>
      consteval size_t CountIndirections() {
         if constexpr (not CT::Complete<T>)
            return 0;
         else if constexpr (CT::Sparse<T>)
            return 1 + CountIndirections<Deptr<T>>();
         else
            return 0;
      }
   }

   /// Strip all qualifiers on all levels of indirection of a type.           
   /// Preserves references, makes them mutable.                              
   /// For example: `void const volatile* const* const` becomes `void**`.     
   ///              `void const volatile* const&` becomes `void*&`.           
   template<class T>
   using DecvqAll = typename decltype(Inner::NestedDecvq<T>())::First;

   /// Adds const qualifiers to all levels of indirection of a type, except   
   /// the top one. You can always do `const ConstAll<T>` to fix that.        
   /// Preserves references, makes them constant.                             
   /// For example: `void**` becomes `void const* const*`.                    
   ///              `void*&` becomes `void const* const&`.                    
   template<class T>
   using ConstAll = typename decltype(Inner::NestedConst<T>())::First;

   /// Removes all bounded array extents from a type.                         
   /// Removes references if type had extent.                                 
   /// For example: `void**(&)[6][6][6]` becomes `void**`.                    
   ///              `void*&` remains `void*&`.                                
   template<class T>
   using DeextAll = typename decltype(Inner::NestedDeext<T>())::First;

   /// Strips all cv-qualifiers from the provided argument                    
   ///   @attention this will return pointers for bounded array arguments     
   template<class T> requires (not ::std::is_bounded_array_v<T>)
   LANGULUS(ALWAYS_INLINED)
   constexpr auto DecvqAllCast(T&& what) noexcept -> DecvqAll<T> {
      if constexpr (CT::Reference<T> or CT::Sparse<T>)
         return const_cast<DecvqAll<T>>(what);
      else
         return LglsFwd(what);
   }
   
   template<class T> requires ::std::is_bounded_array_v<T>
   LANGULUS(ALWAYS_INLINED)
   constexpr auto DecvqAllCast(T&& what) noexcept -> DecvqAll<Deext<T>>* {
      return const_cast<DecvqAll<Deext<T>>*>(what);
   }
   
   /// Add const qualifiers to the provided argument                          
   ///   @attention this will return pointers for bounded array arguments     
   template<class T> requires (not ::std::is_bounded_array_v<T>)
   LANGULUS(ALWAYS_INLINED)
   constexpr auto ConstAllCast(T&& what) noexcept -> ConstAll<T> {
      return const_cast<ConstAll<T>>(what);
   }
   
   template<class T> requires ::std::is_bounded_array_v<T>
   LANGULUS(ALWAYS_INLINED)
   constexpr auto ConstAllCast(T&& what) noexcept -> ConstAll<Deext<T>> const* {
      return const_cast<ConstAll<Deext<T>> const*>(what);
   }
   
   /// Count the number of indirections, including custom pointers.           
   ///   @attention ignores sheddable layers                                  
   template<class T>
   constexpr size_t IndirectsOf = Inner::CountIndirections<T>();

   template<class T, class YES, class NO>
   using Tmut = typename ::std::conditional_t<CT::Mutable<T>,
         ::std::type_identity<YES>,
         ::std::type_identity<NO>
      >::type;

   #define LglsMutIf(CONDITION_TYPE, ...) Tmut<CONDITION_TYPE, __VA_ARGS__, ConstAll<__VA_ARGS__>>

   /// Execute a lambda for each indirection inside a type T                  
   /// The provided lambda must be of the form: [whatever]<class C>{...},     
   /// so that if you provide T as void***, three lambdas will be generated   
   /// and executed, with C being void***, void** and void*.                  
   template<class T>
   void ForEachIndirection(auto&& lambda) {
      if constexpr (CT::Sparse<T>) {
         lambda();
         if constexpr (CT::Sparse<Deptr<T>>)
            ForEachIndirection<Deptr<T>>(LglsFwd(lambda));
      }
   }

   /// Execute a lambda for each indirection inside a type T by dereferencing 
   /// The provided lambda must be of the form: [whatever](auto ptr){...},    
   /// so that if you provide argument as void***, three lambdas will be      
   /// generated and executed, with 'ptr' being void***, void** and void*.    
   template<class T>
   void ForEachIndirection(T& pointer, auto&& lambda) {
      if constexpr (CT::Sparse<T>) {
         lambda(pointer);
         if constexpr (CT::Sparse<Deptr<T>>)
            ForEachIndirection((*pointer), LglsFwd(lambda));
      }
   }
}

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>.         
/// Used to reduce boilerplate.                                               
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention use this macro in the global namespace                       
///   @param NAME the name of the concept - must be the same as the trait in  
///      Langulus::CTTI::NAME                                                 
///   @param HOW how to filter the type when checking it. Use ShedDeref<T>    
///      by default                                                           
#define LANGULUS_CTTI_CONCEPT_INNER(NAME, HOW) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = PartialValidate<T...> \
          and (LANGULUS_CTTI_CHECK(HOW, NAME) and ...); \
      template<class...T> \
      concept Not##NAME = PartialValidate<T...> \
          and ((not LANGULUS_CTTI_CHECK(HOW, NAME)) and ...); \
   }

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>.         
/// Used to reduce boilerplate. Sheds all sheddables and dereferences.        
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention removes all sheddables and dereferences                      
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT(NAME) \
   LANGULUS_CTTI_CONCEPT_INNER(NAME, ShedDeref<T>)

/// MARK: CTTI Macros                                                         
/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>.         
/// Used to reduce boilerplate. Removes only references.                      
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention will only shed references                                    
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT_UNSHEDDABLE(NAME) \
   LANGULUS_CTTI_CONCEPT_INNER(NAME, Deref<T>)

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>.         
/// Used to reduce boilerplate.                                               
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention will shed only references and cv qualifiers                  
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT_UNSHEDDABLE_DECVQ(NAME) \
   LANGULUS_CTTI_CONCEPT_INNER(NAME, Decvq<Deref<T>>)

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>.         
/// Used to reduce boilerplate. Removes qualifiers from argument.             
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention will shed all sheddables, as well as references and cv       
///      qualifiers after that                                                
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT_DECVQ(NAME) \
   LANGULUS_CTTI_CONCEPT_INNER(NAME, Decvq<ShedDeref<T>>)

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>.         
/// Used to reduce boilerplate. Removes qualifiers and extents from argument. 
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention will shed all sheddables, as well as references, cv          
///      qualifiers, and extents after that                                   
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT_DECVQE(NAME) \
   LANGULUS_CTTI_CONCEPT_INNER(NAME, Decvq<DeextAll<ShedDeref<T>>>)

/// Automatically populates the Langulus::CT namespace with the appropriate   
/// concepts, based on the provided Langulus::CTTI::<structure name>.         
/// Used to reduce boilerplate. Decays the argument.                          
///   @attention types need to be complete only if we end up 'delving in'     
///   @attention will shed all sheddables, as well as references, pointers,   
///      and cv qualifiers after that                                         
///   @attention use this macro in the global namespace                       
#define LANGULUS_CTTI_CONCEPT_DECAY(NAME) \
   LANGULUS_CTTI_CONCEPT_INNER(NAME, Decay<T>)

LANGULUS_CTTI_CONCEPT(Null);
LANGULUS_CTTI_CONCEPT(Enum);
LANGULUS_CTTI_CONCEPT(Aggregate);
LANGULUS_CTTI_CONCEPT(Fundamental);
