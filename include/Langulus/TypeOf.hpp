///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Typenav.hpp"


namespace Langulus::CT::Inner
{

   /// Helper function to extract underlying type                             
   /// Supports underlying typelists as well                                  
   template<class T>
   consteval CT::Typelist auto GetUnderlyingType() {
      static_assert(CT::NotReference<T>, "Strip references");

      if constexpr (Array<T>)
         return Types<Deext<T>> {};
      else {
         if constexpr (NotVoid<typename CTTI::Typed<T>::Type>) {
            // Checked externally, T doesn't have to be complete        
            using TLIST = typename CTTI::Typed<T>::Type;
            if constexpr (CT::Typelist<TLIST>)
               return TLIST {};
            else
               return Types<TLIST> {};
         }
         else if constexpr (requires { typename T::CTTI_Typed; }) {
            // Checked internally, T has to be a complete type          
            using TLIST = typename T::CTTI_Typed;
            if constexpr (CT::Typelist<TLIST>)
               return TLIST {};
            else
               return Types<TLIST> {};
         }
         else if constexpr (requires { typename T::value_type; }) {
            // Checked internally, T has to be a complete type          
            using TLIST = typename T::value_type;
            if constexpr (CT::Typelist<TLIST>)
               return TLIST {};
            else
               return Types<TLIST> {};
         }
         else if constexpr (Enum<T>)
            return Types<::std::underlying_type_t<T>> {};
         else
            return Types<void> {};
      }
   };

} // namespace Langulus::CT::Inner

namespace Langulus
{

   /// Get the type that wraps std::underlying_type_t<T> for enums,           
   /// as well as any bounded array, or anything with CTTI::Typed::Type or    
   /// T::CTTI_Typed/T::value_type that isn't 'void'. Will result int a type  
   /// list if inner type contains more than one type                         
   ///   - if T is an array -> return the type (remove extents and refs)      
   ///   - if T has CTTI::Typed is specialized -> return CTTI::Typed::Type    
   ///   - if T has CTTI_Typed/value_type -> return the inner type(s)         
   ///   - if T is an enum -> return the underlying type                      
   ///   - otherwise just return a void type                                  
   template<class T, CT::Typelist INNER = decltype(CT::Inner::GetUnderlyingType<Deref<T>>())>
   using TypeOf = Tif<INNER::Count <= 1, typename INNER::First, INNER>;

   namespace CT
   {

      /// Check if all T are typed by searching for CTTI::Typed<T>            
      /// specializations, or T::CTTI_Typed / T::value_type members           
      ///   @attention the inner type must not be 'void', in order for T to   
      ///      be considered 'typed' (as opposed to 'type-erased')            
      ///   @attention if the inner type is a typelist, that typelist will be 
      ///      accounted for, and Ts are multiply-typed (like TPair)          
      template<class...T>
      concept Typed = (NotVoid<TypeOf<Deref<T>>> and ...);

      /// Check if all T have no underlying types defined                     
      template<class...T>
      concept Untyped = ((not Typed<Deref<T>>) and ...);

   } // namespace Langulus::CT


   /// Downcasts a typed wrapper to the contained element, if cast operator   
   /// to TypeOf<T> is available                                              
   ///  - if T isn't typed, just return itself                                
   ///  - otherwise:                                                          
   ///      -      if T::TypedCast() exists use that                          
   ///      - else if T::operator TypeOf<T>&& exists use that                 
   ///      - else if T::operator TypeOf<T>& exists use that                  
   ///      - else if T::operator const TypeOf<T>& exists use that            
   ///      - else if T::operator TypeOf<T> exists use that                   
   ///   @param what - the instance to decay                                  
   ///   @return the inner data                                               
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr decltype(auto) TypedCast(T&& what) {
      using TT = TypeOf<T>;

      if constexpr (CT::Void<TT>)
         return FWD(what);
      else if constexpr (requires { what.TypedCast(); })
         return what.TypedCast();
      else if constexpr (CT::Typelist<TT>)
         static_assert(false , "Can't decide which type to cast to - add a TypedCast() method to disambiguate");
      else if constexpr (requires { what.operator TT (); })
         return what.operator TT ();
      else
         static_assert(false, "No cast operator available for decaying to inner type");
   }

   /// Strips all sheddable layers down to the most inner type                
   template<class T>
   constexpr decltype(auto) ShedCast(T&& item) noexcept {
      if constexpr (CT::Sheddable<T>) {
         using InnerT = TypeOf<T>;
         static_assert(not ::std::same_as<::std::decay_t<T>, ::std::decay_t<InnerT>>,
            "Sheddable type's inner type is the same, and will result in infinite regress");
         static_assert(requires { static_cast<InnerT>(item); },
            "Sheddable can't be static_casted to the inner type");
         return ShedCast(static_cast<InnerT>(item));
      }
      else return FWD(item);
   };
   
   /// Always returns a pointer to the argument                               
   template<class T>
   constexpr decltype(auto) SparseCast(T&& a) noexcept {
      if constexpr (::std::is_pointer_v<Shed<T>>)
         return  ShedCast(FWD(a));
      else
         return &ShedCast(FWD(a));
   }

   /// Always returns a value reference to the argument                       
   /// If argument is an array, return a value reference to the first element 
   template<class T>
   constexpr decltype(auto) DenseCast(T&& a) {
      if constexpr (CT::Array<Shed<T>>)
         return DenseCast(ShedCast(FWD(a))[0]);
      else if constexpr (CT::Sparse<Shed<T>>)
         // Security is on you - call can throw                         
         return DenseCast(*ShedCast(FWD(a)));
      else
         return ShedCast(FWD(a));
   }

} // namespace Langulus
