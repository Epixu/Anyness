///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "HandlePair.hpp"
#include <Langulus/Retype.hpp>


namespace Langulus::Anyness::Inner
{
   template<CT::NotVoid K, CT::NotVoid V>
   requires (CT::NotHandle<K, V> and CT::NotReference<K, V>)
   using TPairBase = Com::Container<
      Com::Multitype<Com::TypedStatic<DMeta, K, 0>,
                     Com::TypedStatic<DMeta, V, 1>>,
      Com::Multiprovider<Com::Stack<K, 0>,
                         Com::Stack<V, 1>>,
      Com::CountStatic<1u, 0, 1>,         // Statically sized to 1      
      Com::ReserveStatic<1u, 0, 1>,       // Statically reserved to 1   
      Com::OwnershipEmergent<Com::NoOwnership, 0, 1>,
      Com::OwnershipDeepEmergent<true, 0, 1>,
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
      using CTTI_ReflectAs = TPair;
      using CTTI_Deep      = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_MapsTo    = Text;
      //using CTTI_Typed     = Types<K, V>;

      static constexpr bool TypeErased = false;

      using Base     = Inner::TPairBase<K, V>;
      using DeepType = Any;
      using KeyType  = K;
      using ValType  = V;

      using HandleType     = THandlePair<THandleEmergent<ConstAll<K&>>, THandleEmergent<ConstAll<V&>>>;
      using HandleMutType  = THandlePair<THandleEmergent<K&>,           THandleEmergent<V&>>;
      using Pick           = HandleType;
      using PickMut        = HandleMutType;

      constexpr TPair() noexcept requires CT::NotReference<K, V> {
         this->ConstructDefault();
      }
      constexpr TPair(TPair const& other) requires CT::NotReference<K, V> {
         this->Absorb(Refer(other));
      }
      constexpr TPair(TPair&& other) noexcept requires CT::NotReference<K, V> {
         this->Absorb(Move(other));
      }
      constexpr ~TPair() noexcept requires CT::NotReference<K, V> {
         this->Destroy();
      }
      constexpr ~TPair() noexcept requires CT::Reference<K, V> {}

      /// Manual constructor                                                  
      constexpr TPair(CT::NotHandle auto&& a1, CT::NotHandle auto&& a2)
         : Base {Stackwise, LglsFwd(a1), LglsFwd(a2)} {}

      constexpr TPair(CT::Handle auto&& a1, CT::Handle auto&& a2) {
         this->template EmplaceWithIntent<0>(FWDIntent(a1));
         this->template EmplaceWithIntent<1>(FWDIntent(a2));
      }
      constexpr TPair(Inner::Piecewise, CT::NotHandle auto&& a1, CT::NotHandle auto&& a2)
         : Base{Stackwise, LglsFwd(a1), LglsFwd(a2)} {
      }
      constexpr TPair(Inner::Piecewise, CT::NotHandle auto&& a1)
         : Base{Stackwise, LglsFwd(a1), {}} {
      }

      /// Construction that absorbs the provided pair                         
      constexpr TPair(Inner::Absorb, CT::Pair auto&& pair) {
         this->Absorb(LglsFwd(pair));
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

      auto& GetKey(this auto&& self) noexcept {
         return *self.Com::template Stack<K, 0>::Get();
      }
      auto& GetVal(this auto&& self) noexcept {
         return *self.Com::template Stack<V, 1>::Get();
      }

      decltype(auto) GetKeyHandle(this auto&& self) noexcept {
         return self.GetHandle().GetKey(); //TODO use PickDimension instead?
      }
      decltype(auto) GetValHandle(this auto&& self) noexcept {
         return self.GetHandle().GetVal(); //TODO use PickDimension instead?
      }
   };

   template<CT::NotHandle K, CT::NotHandle V>
   TPair(K&&, V&&) -> TPair<Decvq<Deref<Deint<K>>>, Decvq<Deref<Deint<V>>>>;

   template<CT::Handle K, CT::Handle V>
   TPair(K&&, V&&) -> TPair<TypeOf<Deint<K>>, TypeOf<Deint<V>>>;
}

namespace Langulus::CTTI
{
   /// Convert TPair -> Text                                                  
   template<class K, class V>
   struct Converter<Anyness::TPair<K, V>, Anyness::Text> {
      static constexpr auto Convert(Anyness::TPair<K, V> const&) -> Anyness::Text;
   };
}
