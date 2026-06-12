///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include <source/components/Typed-Stack.hpp>
#include <source/components/Multitype.hpp>
#include <source/components/Heap-Movable.hpp>
#include <source/components/Count-Static.hpp>
#include <source/components/Reserve-Static.hpp>
#include <source/components/Ownership-Stack.hpp>
#include <source/components/OwnershipDeep-Heap.hpp>
#include <source/components/Hash-Emergent.hpp>
#include <source/components/Emplacement.hpp>
#include <source/components/Assignment.hpp>
#include <source/components/Removal.hpp>
#include <source/components/Conversion.hpp>
#include <source/components/Comparison.hpp>
#include <source/components/State-Stack.hpp>
#include <source/states/Typed.hpp>
#include <source/states/Future.hpp>
#include <source/states/Past.hpp>
#include <source/states/Compressed.hpp>
#include <source/states/Encrypted.hpp>
#include <source/states/Tracked.hpp>
#include "Handle.hpp"


namespace Langulus::Anyness::Inner
{
   using PairBase = Com::Container<
      Com::Multitype<Com::TypedStack<DMeta, void, false, 0>,
                     Com::TypedStack<DMeta, void, false, 1>>,
      Com::HeapMovable<0, 0, HeapEntry<0>, HeapEntry<1>>,
      Com::CountStatic<1u, 0, 1>,         // Statically sized to 1      
      Com::ReserveStatic<1u, 0, 1>,       // Statically reserved to 1   
      Com::OwnershipStack<Com::StrongOwnership, 0, 1>,
      Com::OwnershipDeepHeap<Com::StrongOwnership, true, 0, 1>, // Deep ownership             
      Com::HashEmergent<0, Hash, 1>,      // Hash retrieved from items  
      Com::Emplacement<0, 1>,             // Allows emplacement         
      Com::Assignment<0, 1>,              // Allows assignment          
      Com::Removal<0, 1>,                 // Allows clear/reset         
      Com::Conversion<0, 1>,              // Allows conversion          
      Com::Comparison<true, 0, 1>,        // Allows comparisons         
      Com::State::Future<>,               // Toggle future linking      
      Com::State::Past<>                  // Toggle past linking        
   >;
}

namespace Langulus::Anyness
{
   ///                                                                        
   /// A type-erased pair.                                                    
   ///   @attention not binary-compatible with its templated equivalent TPair 
   struct Pair : Inner::PairBase {
      using CTTI_ReflectAs = Pair;
      using CTTI_Deep      = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_MapsTo    = Text;

      //static constexpr bool TypeErased = true;
      //static constexpr bool DeeplyOwned = true;
      static constexpr bool ReferenceElements = true;

      using Base     = Inner::PairBase;
      using DeepType = Any;
      using KeyType  = void;
      using ValType  = void;

      using HandleType    = THandlePair<Handle, Handle>;
      using HandleMutType = THandlePair<Handle, HandleMut>;
      using Pick          = HandleType;
      using PickMut       = HandleMutType;

      constexpr Pair() noexcept {
         this->ConstructDefault();
      }
      constexpr Pair(Pair const& other) {
         this->Absorb(Refer(other));
      }
      constexpr Pair(Pair&& other) noexcept  {
         this->Absorb(Move(other));
      }
      constexpr ~Pair() noexcept {
         this->Destroy();
      }
      
      /// Construction that either absorbs the provided pair                  
      constexpr Pair(CT::Pair auto&& p) {
         this->Absorb(LglsFwd(p));
      }
      
      constexpr Pair(Inner::Absorb, CT::Pair auto&& p) {
         this->Absorb(LglsFwd(p));
      }
      
      /// Construction that emplaces A inside, leaves value as default        
      constexpr Pair(Inner::Piecewise, auto&& a1) {
         this->template EmplaceConstruct<0>(LglsFwd(a1));
         this->template EmplaceDefault<1>();
      }

      constexpr Pair(auto&& a1, auto&& a2) {
         this->template EmplaceConstruct<0>(LglsFwd(a1));
         this->template EmplaceConstruct<1>(LglsFwd(a2));
      }

      /// Assignment                                                          
      constexpr Pair& operator = (Pair const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr Pair& operator = (Pair&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }
      
      constexpr Pair& operator = (CT::Pair auto&& pair) {
         return this->AssignAbsorb(LglsFwd(pair));
      }

      using Com::Comparison<true, 0, 1>::operator <=>;
      using Com::Comparison<true, 0, 1>::operator ==;

      decltype(auto) GetKeyHandle(this auto&& self) noexcept {
         return self.GetHandle().GetKeyHandle(); //TODO use PickDimension instead?
      }
      decltype(auto) GetValHandle(this auto&& self) noexcept {
         return self.GetHandle().GetValHandle(); //TODO use PickDimension instead?
      }
   };

   static_assert(CT::TypeErased<Pair>);
}

namespace Langulus::CTTI
{
   /// Convert Pair -> Text                                                   
   template<>
   struct Converter<Anyness::Pair, Anyness::Text> {
      static constexpr auto Convert(Anyness::Pair const&) -> Anyness::Text;
   };
}
