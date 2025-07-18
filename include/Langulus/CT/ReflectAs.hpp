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
   /// Can be used in two ways to satisfy CT::ReflectAs<T>:                   
   /// 1. Specialize for T/concept with Enabled set to true and desired Type  
   /// 2. Add a public `using CTTI_ReflectAs = <DesiredType>;` in T           
   /// Use void/No type to disable reflection for the type                    
   template<class T>
   struct ReflectAs {
      using Type = T;
      static constexpr bool Enabled = false;
   };

   /// nullptr_t is not reflectable                                           
   template<>
   struct ReflectAs<nullptr_t> {
      using Type = void;
      static constexpr bool Enabled = true;
   };
}

namespace Langulus::CT
{
   namespace Inner
   {
      /// Convenience function that checks if ReflectAs is not void, which    
      /// would mean that the type is not reflectable at all                  
      template<CT::NotReference T>
      consteval auto IsReflectable() {
         using DT = Decay<T>;

         if constexpr (Void<T>) {
            // Void is never reflectable                                
            return static_cast<void*>(nullptr);
         }
         else if constexpr (CTTI::ReflectAs<T>::Enabled) {
            // T is checked for safety, so it has to be complete        
            using AS = typename CTTI::ReflectAs<T>::Type;

            if constexpr (Void<AS>)
               return static_cast<void*>(nullptr);
            else {
               static_assert(sizeof(T) == sizeof(AS),
                  "Provided ReflectAs type must be binary compatible");
               return static_cast<AS*>(nullptr);
            }
         }
         else if constexpr (Dense<T> and requires { typename DT::CTTI_ReflectAs; }) {
            // T is checked for safety, so it has to be complete        
            using AS = typename DT::CTTI_ReflectAs;

            if constexpr (Void<AS>)
               return static_cast<void*>(nullptr);
            else {
               static_assert(sizeof(DT) == sizeof(AS),
                  "Provided ReflectAs type must be binary compatible");
               return static_cast<AS*>(nullptr);
            }
         }
         else return static_cast<T*>(nullptr);
      }
   }

   /// Check if all of the types are reflectable                              
   template<class...T>
   concept Reflectable = Inner::CheckSize<T...>()
       and (CT::NotVoid<Deptr<decltype(Inner::IsReflectable<Deref<T>>())>> and ...);

   /// Get the type a given type is reflected as. This is very useful as a    
   /// a build-time optimization, because many type-erased containers are     
   /// binary-compatible with their templated equivalents, and the use of     
   /// CTTI_ReflectAs can drastically lower build time for meta generation,   
   /// by reducing unnessesary template instantiations of redundant types     
   template<class T>
   using ReflectedAs = Deptr<decltype(Inner::IsReflectable<Deref<T>>())>;
}
