///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Pair.hpp"
#include <source/components/Multiprovider.hpp>
#include <source/components/Multiown-Deep.hpp>


namespace Langulus::Anyness::Inner
{
   /// MARK: Bases                                                            
   /// Stack based pair. Supports references.                                 
   ///   @attention not binary compatible with Pair                           
   template<CT::NotVoid K, CT::NotVoid V> requires CT::NotHandle<K, V>
   using TPairStackBase = Com::Container<
      Com::Multitype    <Com::TypedStatic<DMeta, Deref<K>, 0>,
                         Com::TypedStatic<DMeta, Deref<V>, 1>>,
      Com::Multiprovider<Com::Stack<K, 0>,
                         Com::Stack<V, 1>>,
      Com::CountStatic<1u, 0, 1>,         // Statically sized to 1      
      Com::ReserveStatic<1u, 0, 1>,       // Statically reserved to 1   
      Com::OwnershipDeepEmergent<Com::StrongOwnership, true, 0, 1>,
      Com::HashEmergent<0, Hash, 1>,      // Hash retrieved from items  
      Com::Emplacement<0, 1>,             // Allows emplacement         
      Com::Assignment<0, 1>,              // Allows assignment          
      Com::Removal<0, 1>,                 // Allows clear/reset         
      Com::Conversion<0, 1>,              // Allows conversion          
      Com::Comparison<true, 0, 1>         // Allows comparisons         
   >;

   /// Heap based pair. Binary compatible with Pair.                          
   ///   @attention does not support references                               
   template<CT::NotVoid K, CT::NotVoid V>
   requires (CT::NotHandle<K, V> and CT::NotReference<K, V>)
   using TPairHeapBase = Com::Container<
      Com::Multitype<Com::TypedStack<DMeta, K, true, 0>,
                     Com::TypedStack<DMeta, V, true, 1>>,
      Com::HeapMovable<0, 0, HeapEntry<0, K*>, HeapEntry<1, V*>>,
      Com::CountStatic<1u, 0, 1>,         // Statically sized to 1      
      Com::ReserveStatic<1u, 0, 1>,       // Statically reserved to 1   
      Com::OwnershipStack<Com::StrongOwnership, 0, 1>,
      Com::MultiownDeep<EnableComponentIf<CT::Sparse<K>, Com::OwnershipDeepHeap<Com::StrongOwnership, true, 0>>,
                        EnableComponentIf<CT::Sparse<V>, Com::OwnershipDeepHeap<Com::StrongOwnership, true, 1>>>,
      Com::HashEmergent<0, Hash, 1>,      // Hash retrieved from items  
      Com::Emplacement<0, 1>,             // Allows emplacement         
      Com::Assignment<0, 1>,              // Allows assignment          
      Com::Removal<0, 1>,                 // Allows clear/reset         
      Com::Conversion<0, 1>,              // Allows conversion          
      Com::Comparison<true, 0, 1>,        // Allows comparisons         
      Com::State::Future<>,               // Toggle future linking      
      Com::State::Past<>,                 // Toggle past linking        
      Com::State::Encrypted<>             // Toggle encryption          
   >;

   template<CT::NotVoid K, CT::NotVoid V> requires CT::NotHandle<K, V>
   using TPairBase = Tif<CT::NotReference<K, V>, TPairHeapBase<Deref<K>, Deref<V>>, TPairStackBase<K, V>>;
}


namespace Langulus::Anyness
{
   /// MARK: TPair                                                            
   ///                                                                        
   /// A statically-typed pair. Supports holding references.                  
   ///   @attention when containing references, this pair is stack-based and  
   ///      binary incompatible with Pair                                     
   template<CT::NotVoid K, CT::NotVoid V>
   struct TPair : Inner::TPairBase<K, V> {
      using CTTI_ReflectAs = TPair;
      using CTTI_Deep      = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_MapsTo    = Text;

      using Base           = Inner::TPairBase<K, V>;
      using DeepType       = Any;
      using HandleType     = Tif<CT::NotReference<K, V>,
         THandlePair<THandle        <ConstAll<K&>>, THandle        <ConstAll<V&>>>,
         THandlePair<THandleEmergent<ConstAll<K&>>, THandleEmergent<ConstAll<V&>>>
      >;
      using HandleMutType  = Tif<CT::NotReference<K, V>,
         THandlePair<THandle        <K&>,  THandle        <V&>>,
         THandlePair<THandleEmergent<K&>,  THandleEmergent<V&>>
      >;

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
      constexpr ~TPair() noexcept {
         this->Destroy();
      }
      
      constexpr TPair(CT::Pair auto&& p) {
         this->Absorb(LglsFwd(p));
      }
      
      constexpr TPair(Inner::Absorb, CT::Pair auto&& p) {
         this->Absorb(LglsFwd(p));
      }

      /// Stack-based constructors                                            
      constexpr TPair(CT::NotHandle auto&& a1, CT::NotHandle auto&& a2)
      requires (not CT::NotReference<K, V>)
         : Base {Stackwise, LglsFwd(a1), LglsFwd(a2)} {
         if constexpr (CT::Sparse<K> or CT::Sparse<V>)
            this->Com::OwnershipDeepEmergent<Com::StrongOwnership, true, 0, 1>::Keep();
      }
      
      constexpr TPair(Inner::Piecewise, CT::NotHandle auto&& a1, CT::NotHandle auto&& a2)
      requires (not CT::NotReference<K, V>)
         : Base{Stackwise, LglsFwd(a1), LglsFwd(a2)} {
         if constexpr (CT::Sparse<K> or CT::Sparse<V>)
            this->Com::OwnershipDeepEmergent<Com::StrongOwnership, true, 0, 1>::Keep();
      }

      constexpr TPair(Inner::Piecewise, CT::NotHandle auto&& a1)
      requires (not CT::NotReference<K, V>)
         : Base{Stackwise, LglsFwd(a1), {}} {
         if constexpr (CT::Sparse<K> or CT::Sparse<V>)
            this->Com::OwnershipDeepEmergent<Com::StrongOwnership, true, 0, 1>::Keep();
      }

      /// Construct from handles                                              
      constexpr TPair(CT::Handle auto&& a1, CT::Handle auto&& a2) {
         this->template EmplaceWithIntent<0>(FWDIntent(a1));
         this->template EmplaceWithIntent<1>(FWDIntent(a2));
      }

      constexpr TPair(CT::NotHandle auto&& a1, CT::NotHandle auto&& a2)
      requires CT::NotReference<K, V> {
         this->template EmplaceWithIntent<0>(FWDIntent(a1));
         this->template EmplaceWithIntent<1>(FWDIntent(a2));
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

      /// Clear the pair and assign a key and a value                         
      constexpr TPair& Assign(auto&& a1, auto&& a2) {
         this->Clear();
         this->template EmplaceConstruct<0>(LglsFwd(a1));
         this->template EmplaceConstruct<1>(LglsFwd(a2));
         return *this;
      }

      using Com::Comparison<true, 0, 1>::operator <=>;
      using Com::Comparison<true, 0, 1>::operator ==;

      auto& GetKey(this auto&& self) noexcept {
         if constexpr (CT::NotReference<K, V>)
            return *self.Com::template HeapMovable<0, 0, HeapEntry<0, K*>, HeapEntry<1, V*>>::template Get<void, 0>();
         else
            return *self.Com::template Stack<K, 0>::Get();
      }

      auto& GetVal(this auto&& self) noexcept {
         if constexpr (CT::NotReference<K, V>)
            return *self.Com::template HeapMovable<0, 0, HeapEntry<0, K*>, HeapEntry<1, V*>>::template Get<void, 1>();
         else
            return *self.Com::template Stack<V, 1>::Get();
      }

      auto GetKeyHandle() const noexcept -> typename HandleType::KeyHandle {
         return {*this};
      }

      auto GetKeyHandle() noexcept -> typename HandleMutType::KeyHandle {
         return {*this};
      }

      auto GetValHandle() const noexcept -> typename HandleType::ValHandle {
         return {Slice<1>, *this};
      }

      auto GetValHandle() noexcept -> typename HandleMutType::ValHandle {
         return {Slice<1>, *this};
      }
   };

   /// MARK: CTAD                                                             
   template<CT::NotHandle K, CT::NotHandle V>
   TPair(K&&, V&&) -> TPair<Decvq<Deref<Deint<K>>>, Decvq<Deref<Deint<V>>>>;

   template<CT::Handle K, CT::Handle V>
   TPair(K&&, V&&) -> TPair<TypeOf<Deint<K>>, TypeOf<Deint<V>>>;
}

namespace Langulus::CTTI
{
   /// MARK: Converters                                                       
   /// Convert TPair -> Text                                                  
   template<class K, class V>
   struct Converter<Anyness::TPair<K, V>, Anyness::Text> {
      static constexpr auto Convert(Anyness::TPair<K, V> const&) -> Anyness::Text;
   };
}
