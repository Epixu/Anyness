///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Handle.hpp"


namespace Langulus::Anyness
{
   /// Type-erased immutable handle                                           
   template<>
   struct THandlePair<Handle, Handle> : Com::Container<
      Com::TypedStack<DMeta, void, false, 0>,
      Com::TypedStack<DMeta, void, false, 1>,
      Com::HeapReference<HeapEntry<0, void*>>,
      Com::HeapReference<HeapEntry<1, void*>>,
      Com::CountStatic<1u, 0, 1>,
      Com::OwnershipDeepReference<true, 0>,
      Com::OwnershipDeepReference<true, 1>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Comparison<0, true, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;

      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = Handle;
      using ValHandle = Handle;

      static constexpr bool TypeErased  = true;
      static constexpr bool DeeplyOwned = true;

      /// Handles can't be piecewise-initialized                              
      THandlePair(Inner::Piecewise, auto&&) = delete;

      constexpr THandlePair() noexcept {
         this->ConstructDefault();
      }

      constexpr THandlePair(THandlePair const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandlePair(THandlePair&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr THandlePair(Handle&& key, Handle&& val) noexcept {
         this->Com::HeapReference<HeapEntry<0, void*>>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<HeapEntry<1, void*>>::SetHeapInner(val.GetHeapInner());
         this->Com::OwnershipDeepReference<true, 0>::SetEntriesInner(key.GetEntriesInner());
         this->Com::OwnershipDeepReference<true, 1>::SetEntriesInner(val.GetEntriesInner());
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(key.GetTypeInner());
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(val.GetTypeInner());
      }

      /// Assignment                                                          
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      Handle GetKey() {
         return {
            this->Com::HeapReference<HeapEntry<0, void*>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<true, 0>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 0>::GetTypeInner()
         };
      }

      Handle GetVal() {
         return {
            this->Com::HeapReference<HeapEntry<1, void*>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<true, 1>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 1>::GetTypeInner()
         };
      }

   //protected:
      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> THandlePair<HandleMut, HandleMut>& {
         return *reinterpret_cast<THandlePair<HandleMut, HandleMut>*>(this);
      }
   };
   
   /// Type-erased mutable handle                                             
   template<>
   struct THandlePair<HandleMut, HandleMut> : Com::Container<
      Com::TypedStack<DMeta, void, false, 0>,
      Com::TypedStack<DMeta, void, false, 1>,
      Com::HeapReference<HeapEntry<0, void*>>,
      Com::HeapReference<HeapEntry<1, void*>>,
      Com::CountStatic<1u, 0, 1>,
      Com::OwnershipDeepReference<true, 0>,
      Com::OwnershipDeepReference<true, 1>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Assignment<0, 1>,
      Com::Emplacement<0, 1>,
      Com::Comparison<0, true, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;

      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = HandleMut;
      using ValHandle = HandleMut;

      static constexpr bool TypeErased  = true;
      static constexpr bool DeeplyOwned = true;

      /// Handles can't be piecewise-initialized                              
      THandlePair(Inner::Piecewise, auto&&) = delete;

      constexpr THandlePair() noexcept {
         this->ConstructDefault();
      }

      constexpr THandlePair(THandlePair const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandlePair(THandlePair&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr THandlePair(HandleMut&& key, HandleMut&& val) noexcept {
         this->Com::HeapReference<HeapEntry<0, void*>>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<HeapEntry<1, void*>>::SetHeapInner(val.GetHeapInner());
         this->Com::OwnershipDeepReference<true, 0>::SetEntriesInner(key.GetEntriesInner());
         this->Com::OwnershipDeepReference<true, 1>::SetEntriesInner(val.GetEntriesInner());
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(key.GetTypeInner());
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(val.GetTypeInner());
      }

      /// Assignment                                                          
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      HandleMut GetKey() {
         return {
            this->Com::HeapReference<HeapEntry<0, void*>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<true, 0>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 0>::GetTypeInner()
         };
      }

      HandleMut GetVal() {
         return {
            this->Com::HeapReference<HeapEntry<1, void*>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<true, 1>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 1>::GetTypeInner()
         };
      }

      /// Already as mutable as it gets                                       
      auto ForceMutable() noexcept -> THandlePair& {
         return *this;
      }
   };
   
   /// Type-erased immutable key paired with mutable value                    
   /// Often used for mutable access in maps, where keys can't be modified    
   template<>
   struct THandlePair<Handle, HandleMut> : Com::Container<
      Com::TypedStack<DMeta, void, false, 0>,
      Com::TypedStack<DMeta, void, false, 1>,
      Com::HeapReference<HeapEntry<0, void*>>,
      Com::HeapReference<HeapEntry<1, void*>>,
      Com::CountStatic<1u, 0, 1>,
      Com::OwnershipDeepReference<true, 0>,
      Com::OwnershipDeepReference<true, 1>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Assignment<1>,
      Com::Emplacement<1>,
      Com::Comparison<0, true, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;

      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = Handle;
      using ValHandle = HandleMut;

      static constexpr bool TypeErased  = true;
      static constexpr bool DeeplyOwned = true;

      /// Handles can't be piecewise-initialized                              
      THandlePair(Inner::Piecewise, auto&&) = delete;

      constexpr THandlePair() noexcept {
         this->ConstructDefault();
      }

      constexpr THandlePair(THandlePair const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandlePair(THandlePair&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr THandlePair(Handle&& key, HandleMut&& val) noexcept {
         this->Com::HeapReference<HeapEntry<0, void*>>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<HeapEntry<1, void*>>::SetHeapInner(val.GetHeapInner());
         this->Com::OwnershipDeepReference<true, 0>::SetEntriesInner(key.GetEntriesInner());
         this->Com::OwnershipDeepReference<true, 1>::SetEntriesInner(val.GetEntriesInner());
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(key.GetTypeInner());
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(val.GetTypeInner());
      }

      /// Assignment                                                          
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      Handle GetKey() {
         return {
            this->Com::HeapReference<HeapEntry<0, void*>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<true, 0>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 0>::GetTypeInner()
         };
      }

      HandleMut GetVal() {
         return {
            this->Com::HeapReference<HeapEntry<1, void*>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<true, 1>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 1>::GetTypeInner()
         };
      }

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept -> THandlePair<HandleMut, HandleMut>& {
         return *reinterpret_cast<THandlePair<HandleMut, HandleMut>*>(this);
      }
   };

   /// Statically typed emergent handles                                      
   template<CT::Reference K, CT::Reference V> requires CT::NotSheddable<K, V>
   struct THandlePair<THandleEmergent<K>, THandleEmergent<V>> : Com::Container<
      Com::TypedStatic<DMeta, Deref<K>, 0>,
      Com::TypedStatic<DMeta, Deref<V>, 1>,
      Com::HeapReference<HeapEntry<0, Deref<K>*>>,
      Com::HeapReference<HeapEntry<1, Deref<V>*>>,
      Com::CountStatic<1u, 0, 1>,
      EnableComponentIf<CT::Dense<K, V>,                 Com::OwnershipEmergent<Com::WeakOwnership, 0, 1>>,
      EnableComponentIf<CT::Dense<K> and CT::Sparse<V>,  Com::OwnershipEmergent<Com::WeakOwnership, 0>>,
      EnableComponentIf<CT::Sparse<K> and CT::Dense<V>,  Com::OwnershipEmergent<Com::WeakOwnership, 1>>,
      EnableComponentIf<CT::Sparse<K>,                   Com::OwnershipDeepEmergent<true, 0>>,
      EnableComponentIf<CT::Sparse<V>,                   Com::OwnershipDeepEmergent<true, 1>>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Comparison<0, true, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;
      using CTTI_Typed     = Types<Deref<K>, Deref<V>>;

      using Denser    = THandlePair<typename THandleEmergent<K>::Denser, typename THandleEmergent<V>::Denser>;
      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = THandleEmergent<K>;
      using ValHandle = THandleEmergent<V>;

      static constexpr bool Emergent = true;

      /// Handles can't be piecewise-initialized                              
      THandlePair(Inner::Piecewise, auto&&) = delete;

      constexpr THandlePair() noexcept {
         this->ConstructDefault();
      }

      constexpr THandlePair(THandlePair const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandlePair(THandlePair&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr THandlePair(THandleEmergent<K>&& key, THandleEmergent<V>&& val) noexcept {
         this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::SetHeapInner(val.GetHeapInner());
      }

      /// Assignment                                                          
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      THandleEmergent<K> GetKey() {
         return {this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::GetHeapInner()};
      }

      THandleEmergent<V> GetVal() {
         return {this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::GetHeapInner()};
      }

   //protected:
      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept
         -> THandlePair<THandleEmergent<Decvq<Deref<K>>&>,
                        THandleEmergent<Decvq<Deref<V>>&>>&
      {
         return *reinterpret_cast<THandlePair<
            THandleEmergent<Decvq<Deref<K>>&>,
            THandleEmergent<Decvq<Deref<V>>&>
         >*>(this);
      }
   };

   /// Statically typed embedded handles                                      
   template<CT::Reference K, CT::Reference V> requires CT::NotSheddable<K, V>
   struct THandlePair<THandle<K>, THandle<V>> : Com::Container<
      Com::TypedStatic<DMeta, Deref<K>, 0>,
      Com::TypedStatic<DMeta, Deref<V>, 1>,
      Com::HeapReference<HeapEntry<0, Deref<K>*>>,
      Com::HeapReference<HeapEntry<1, Deref<V>*>>,
      Com::CountStatic<1u, 0, 1>,
      EnableComponentIf<CT::Dense<K, V>,                 Com::OwnershipStack<Com::WeakOwnership, 0, 1>>,
      EnableComponentIf<CT::Dense<K> and CT::Sparse<V>,  Com::OwnershipStack<Com::WeakOwnership, 0>>,
      EnableComponentIf<CT::Sparse<K> and CT::Dense<V>,  Com::OwnershipStack<Com::WeakOwnership, 1>>,
      EnableComponentIf<CT::Sparse<K>,                   Com::OwnershipDeepReference<true, 0>>,
      EnableComponentIf<CT::Sparse<V>,                   Com::OwnershipDeepReference<true, 1>>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Assignment<0, 1>,
      Com::Emplacement<0, 1>,
      Com::Comparison<0, true, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;
      using CTTI_Typed     = Types<Deref<K>, Deref<V>>;

      using Denser    = THandlePair<typename THandle<K>::Denser, typename THandle<V>::Denser>;
      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = THandle<K>;
      using ValHandle = THandle<V>;

      static constexpr bool TypeErased = false;
      static constexpr bool DeeplyOwned = CT::Sparse<K> or CT::Sparse<V>;
      static constexpr bool ReferenceElements = true;

      /// Handles can't be piecewise-initialized                              
      THandlePair(Inner::Piecewise, auto&&) = delete;

      constexpr THandlePair() noexcept {
         this->ConstructDefault();
      }

      constexpr THandlePair(THandlePair const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandlePair(THandlePair&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr THandlePair(THandle<K>&& key, THandle<V>&& val) noexcept {
         this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::SetHeapInner(val.GetHeapInner());
      }

      /// Assignment                                                          
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      auto GetKey() -> KeyHandle {
         if constexpr (CT::Dense<K, V>) {
            return {
               this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::GetHeapInner(),
               this->Com::OwnershipStack<Com::WeakOwnership, 0, 1>::GetAllocation()
            };
         }
         else if constexpr (CT::Dense<K>) {
            return {
               this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::GetHeapInner(),
               this->Com::OwnershipStack<Com::WeakOwnership, 0>::GetAllocation()
            };
         }
         else {
            return {
               this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::GetHeapInner(),
               this->Com::OwnershipDeepReference<true, 0>::GetEntriesInner()
            };
         }
      }

      auto GetVal() -> ValHandle {
         if constexpr (CT::Dense<K, V>) {
            return {
               this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::GetHeapInner(),
               this->Com::OwnershipStack<Com::WeakOwnership, 0, 1>::GetAllocation()
            };
         }
         else if constexpr (CT::Dense<V>) {
            return {
               this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::GetHeapInner(),
               this->Com::OwnershipStack<Com::WeakOwnership, 1>::GetAllocation()
            };
         }
         else {
            return {
               this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::GetHeapInner(),
               this->Com::OwnershipDeepReference<true, 1>::GetEntriesInner()
            };
         }
      }

   //protected:
      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept
         -> THandlePair<THandle<Decvq<Deref<K>>&>,
                        THandle<Decvq<Deref<V>>&>>&
      {
         return *reinterpret_cast<THandlePair<
            THandle<Decvq<Deref<K>>&>,
            THandle<Decvq<Deref<V>>&>
         >*>(this);
      }
   };

   /// Statically typed local handles                                         
   template<CT::NotReference K, CT::NotReference V> requires (CT::NotSheddable<K, V> and CT::NotHandle<K, V>)
   struct THandlePair<THandle<K>, THandle<V>> : Com::Container<
      Com::TypedStatic<DMeta, Deref<K>, 0>,
      Com::TypedStatic<DMeta, Deref<V>, 1>,
      EnableComponentIf<CT::Dense<K>, Com::Stack<K, 0>>,
      EnableComponentIf<CT::Dense<V>, Com::Stack<V, 1>>,
      EnableComponentIf<CT::Sparse<K>, Com::HeapMovable<0, 0, HeapEntry<0, K*>>>,
      EnableComponentIf<CT::Sparse<V>, Com::HeapMovable<0, 0, HeapEntry<1, V*>>>,
      Com::CountStatic<1u, 0, 1>,
      EnableComponentIf<CT::Sparse<K, V>,                Com::ReserveEmergent<size_t, 0, 1>>,
      EnableComponentIf<CT::Sparse<K> and CT::Dense<V>,  Com::ReserveEmergent<size_t, 0>>,
      EnableComponentIf<CT::Dense<K> and CT::Sparse<V>,  Com::ReserveEmergent<size_t, 1>>,
      EnableComponentIf<CT::Sparse<K, V>,                Com::OwnershipStack<Com::StrongOwnership, 0, 1>>,
      EnableComponentIf<CT::Sparse<K> and CT::Dense<V>,  Com::OwnershipStack<Com::StrongOwnership, 0>>,
      EnableComponentIf<CT::Dense<K> and CT::Sparse<V>,  Com::OwnershipStack<Com::StrongOwnership, 1>>,
      EnableComponentIf<CT::Sparse<K, V>,                Com::OwnershipDeepHeap<true, 0, 1>>,
      EnableComponentIf<CT::Sparse<K> and CT::Dense<V>,  Com::OwnershipDeepHeap<true, 0>>,
      EnableComponentIf<CT::Dense<K> and CT::Sparse<V>,  Com::OwnershipDeepHeap<true, 1>>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Assignment<0, 1>,
      Com::Emplacement<0, 1>,
      Com::Comparison<0, true, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;
      using CTTI_Typed     = Types<Deref<K>, Deref<V>>;

      using Denser    = THandlePair<typename THandle<K>::Denser, typename THandle<V>::Denser>;
      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = Tif<CT::Sparse<K>, THandle<K&>, THandleEmergent<K&>>;
      using ValHandle = Tif<CT::Sparse<V>, THandle<V&>, THandleEmergent<V&>>;

      static constexpr bool TypeErased = false;
      static constexpr bool DeeplyOwned = CT::Sparse<K> or CT::Sparse<V>;
      static constexpr bool ReferenceElements = true;

      /// Handles can't be piecewise-initialized                              
      THandlePair(Inner::Piecewise, auto&&) = delete;

      constexpr THandlePair() noexcept {
         this->ConstructDefault();
      }

      constexpr THandlePair(THandlePair const& other) {
         this->Absorb(Refer(other));
      }

      constexpr THandlePair(THandlePair&& other) noexcept {
         this->Absorb(Move(other));
      }

      constexpr THandlePair(auto&& key, auto&& val) noexcept {
         this->Com::Emplacement<0, 1>::template EmplaceConstruct<0>(LglsFwd(key));
         this->Com::Emplacement<0, 1>::template EmplaceConstruct<1>(LglsFwd(val));
      }

      /// Assignment                                                          
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      auto GetKey() -> KeyHandle {
         if constexpr (CT::Sparse<K, V>) {
            return THandle<K&> {
               this->Com::HeapMovable<0, 0, HeapEntry<0, K*>>::GetRaw(),
               this->Com::OwnershipDeepHeap<true, 0, 1>::template GetEntriesInner<0>()
            };
         }
         else if constexpr (CT::Sparse<K>) {
            return THandle<K&> {
               this->Com::HeapMovable<0, 0, HeapEntry<0, K*>>::GetRaw(),
               this->Com::OwnershipDeepHeap<true, 0>::GetEntriesInner()
            };
         }
         else return THandleEmergent<K&> {this->Com::Stack<K, 0>::GetRaw()};
      }

      auto GetVal() -> ValHandle {
         if constexpr (CT::Sparse<K, V>) {
            return THandle<V&> {
               this->Com::HeapMovable<0, 0, HeapEntry<1, V*>>::GetRaw(),
               this->Com::OwnershipDeepHeap<true, 0, 1>::template GetEntriesInner<1>()
            };
         }
         else if constexpr (CT::Sparse<V>) {
            return THandle<V&> {
               this->Com::HeapMovable<0, 0, HeapEntry<1, V*>>::GetRaw(),
               this->Com::OwnershipDeepHeap<true, 1>::GetEntriesInner()
            };
         }
         else return THandleEmergent<V&> {this->Com::Stack<V, 1>::GetRaw()};
      }

   //protected:
      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() noexcept  -> THandlePair<THandle<Decvq<K>>, THandle<Decvq<V>>>& {
         return *reinterpret_cast<THandlePair<THandle<Decvq<K>>, THandle<Decvq<V>>>*>(this);
      }
   };

   template<CT::Handle K, CT::Handle V>
   THandlePair(K&&, V&&) -> THandlePair<Decay<K>, Decay<V>>;
}
