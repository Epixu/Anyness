///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Handle.hpp"
#include <Langulus/Retype.hpp>


namespace Langulus::Anyness::Inner
{
   template<CT::NotVoid K, CT::NotVoid V>
   using TPairBase = Com::Container<
      Com::TypedStatic<DMeta, K, 0>,
      Com::TypedStatic<DMeta, V, 1>,
      Com::Stack<K, 0>,
      Com::Stack<V, 1>,
      Com::CountStatic<0, 1u, 1>,         // Statically sized to 1      
      Com::ReserveStatic<0, 1u, 1>,       // Statically reserved to 1   
      Com::OwnershipDeepEmergent<0>,      // Separate key deep ownership
      Com::OwnershipDeepEmergent<1>,      // Separate val deep onwership
      Com::HashEmergent<0, Hash, 1>,      // Hash retrieved from items  
      Com::Emplacement<0, 1>,             // Allows emplacement         
      Com::Assignment<0, 1>,              // Allows assignment          
      Com::Removal<0, 1>,                 // Allows clear/reset         
      Com::Conversion<0, 1>,              // Allows conversion          
      Com::Comparison<0, true, 1>         // Allows comparisons         
   >;
}


namespace Langulus::Anyness
{
   ///                                                                        
   /// A statically-typed pair. Supports holding references.                  
   ///   @attention not-binary compatible with its type-erased Pair           
   template<CT::NotVoid K, CT::NotVoid V>
   struct TPair : Inner::TPairBase<K, V> {
      using CTTI_Deep   = Yes<>;
      using CTTI_Pair   = Yes<>;
      using CTTI_MapsTo = Text;
      using CTTI_Typed  = Types<K, V>;

      static constexpr bool TypeErased = false;

      using Base     = Inner::TPairBase<K, V>;
      using DeepType = Any;
      using KeyType  = K;
      using ValType  = V;

      constexpr TPair() noexcept requires CT::NotReference<K, V> {
         this->ConstructDefault();
      }
      constexpr TPair(TPair const& other) requires CT::NotReference<K, V> {
         this->Absorb(Refer(other));
      }
      constexpr TPair(TPair&& other) noexcept requires CT::NotReference<K, V> {
         this->Absorb(Move(other));
      }
      constexpr TPair(auto&& a1, auto&& a2) requires CT::NotReference<K, V> {
         this->template EmplaceConstruct<0>(LglsFwd(a1));
         this->template EmplaceConstruct<1>(LglsFwd(a2));
      }
      constexpr ~TPair() noexcept requires CT::NotReference<K, V> {
         this->Destroy();
      }

      /// Reference constructor                                               
      constexpr TPair(auto&& a1, auto&& a2) requires CT::Reference<K, V>
         : Base {Stackwise, LglsFwd(a1), LglsFwd(a2)} {}

      constexpr ~TPair() noexcept requires CT::Reference<K, V> {}

      /// Construction that absorbs the provided pair                         
      constexpr TPair(Inner::Absorb, CT::Pair auto&& pair) {
         this->Absorb(LglsFwd(pair));
      }
      
      /// Construction that emplaces A inside, leaves value as default        
      constexpr TPair(Inner::Piecewise, auto&& a1) {
         this->template EmplaceConstruct<0>(LglsFwd(a1));
         this->template EmplaceDefault<1>();
      }
      
      /// Assignment                                                          
      constexpr TPair& operator = (TPair const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr TPair& operator = (TPair&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }
      
      constexpr TPair& operator = (CT::Pair auto&& pair) {
         return this->AssignAbsorb(LglsFwd(pair));
      }

      using Com::Comparison<0, true, 1>::operator <=>;
      using Com::Comparison<0, true, 1>::operator ==;
   };
}
