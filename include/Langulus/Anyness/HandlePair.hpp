///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Handle.hpp"
#include <source/components/Multitype.hpp>
#include <source/components/Multiprovider.hpp>
#include <source/components/Multiown.hpp>
#include <source/components/Multiown-Deep.hpp>


namespace Langulus::Anyness
{
   ///                                                                        
   /// Type-erased immutable handle                                           
   ///                                                                        
   template<>
   struct THandlePair<Handle, Handle> : Com::Container<
      Com::Multitype<Com::TypedStack<DMeta, void, false, 0>,
                     Com::TypedStack<DMeta, void, false, 1>>,
      Com::Multiprovider<Com::HeapReference<HeapEntry<0>>,
                         Com::HeapReference<HeapEntry<1>>>,
      Com::CountStatic<1u, 0, 1>,
      Com::MultiownDeep<Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>,
                        Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Comparison<true, 0, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;

      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = Handle;
      using ValHandle = Handle;

      //static constexpr bool TypeErased    = true;
      //static constexpr bool DeeplyOwned   = true;
      //static constexpr bool HeapCanBeNull = true;

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
         this->Com::HeapReference<HeapEntry<0>>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<HeapEntry<1>>::SetHeapInner(val.GetHeapInner());
         this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>::SetEntriesInner(key.GetEntriesInner());
         this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>::SetEntriesInner(val.GetEntriesInner());
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(key.GetTypeInner());
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(val.GetTypeInner());
      }

      /// Assignment is disabled                                              
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      Handle GetKey() noexcept {
         return {
            this->Com::HeapReference<HeapEntry<0>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 0>::GetTypeInner()
         };
      }
      Handle GetKeyHandle() noexcept {
         return GetKey();
      }

      Handle GetVal() noexcept {
         return {
            this->Com::HeapReference<HeapEntry<1>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 1>::GetTypeInner()
         };
      }
      Handle GetValHandle() noexcept {
         return GetVal();
      }

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      decltype(auto) ForceMutable(this auto&& self) noexcept {
         return reinterpret_cast<THandlePair<HandleMut, HandleMut>&&>(LglsFwd(self));
      }

      /// Pick a specific dimension                                           
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
              if constexpr (SID == 0)  return self.GetKeyHandle();
         else if constexpr (SID == 1)  return self.GetValHandle();
         else static_assert(false, "No such dimension");
      }
   };
   

   ///                                                                        
   /// Type-erased mutable handle                                             
   ///                                                                        
   template<>
   struct THandlePair<HandleMut, HandleMut> : Com::Container<
      Com::Multitype<Com::TypedStack<DMeta, void, false, 0>,
                     Com::TypedStack<DMeta, void, false, 1>>,
      Com::Multiprovider<Com::HeapReference<HeapEntry<0>>,
                         Com::HeapReference<HeapEntry<1>>>,
      Com::CountStatic<1u, 0, 1>,
      Com::MultiownDeep<Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>,
                        Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Assignment<0, 1>,
      Com::Emplacement<0, 1>,
      Com::Comparison<true, 0, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;

      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = HandleMut;
      using ValHandle = HandleMut;

      //static constexpr bool TypeErased    = true;
      //static constexpr bool DeeplyOwned   = true;
      //static constexpr bool HeapCanBeNull = true;

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
         this->Com::HeapReference<HeapEntry<0>>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<HeapEntry<1>>::SetHeapInner(val.GetHeapInner());
         this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>::SetEntriesInner(key.GetEntriesInner());
         this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>::SetEntriesInner(val.GetEntriesInner());
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(key.GetTypeInner());
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(val.GetTypeInner());
      }

      /// Assignment is disabled                                              
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      HandleMut GetKey() noexcept {
         return {
            this->Com::HeapReference<HeapEntry<0>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 0>::GetTypeInner()
         };
      }

      HandleMut GetKeyHandle() noexcept {
         return GetKey();
      }

      HandleMut GetVal() noexcept {
         return {
            this->Com::HeapReference<HeapEntry<1>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 1>::GetTypeInner()
         };
      }
      HandleMut GetValHandle() noexcept {
         return GetVal();
      }

      /// Already as mutable as it gets                                       
      decltype(auto) ForceMutable(this auto&& self) noexcept {
         return LglsFwd(self);
      }

      /// Pick a specific dimension                                           
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
              if constexpr (SID == 0)  return self.GetKeyHandle();
         else if constexpr (SID == 1)  return self.GetValHandle();
         else static_assert(false, "No such dimension");
      }
   };
   

   ///                                                                        
   /// Type-erased immutable key paired with mutable value.                   
   /// Often used for mutable access in maps, where keys can't be modified.   
   template<>
   struct THandlePair<Handle, HandleMut> : Com::Container<
      Com::Multitype<Com::TypedStack<DMeta, void, false, 0>,
                     Com::TypedStack<DMeta, void, false, 1>>,
      Com::Multiprovider<Com::HeapReference<HeapEntry<0>>,
                         Com::HeapReference<HeapEntry<1>>>,
      Com::CountStatic<1u, 0, 1>,
      Com::MultiownDeep<Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>,
                        Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Assignment<1>,
      Com::Emplacement<1>,
      Com::Comparison<true, 0, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;

      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = Handle;
      using ValHandle = HandleMut;

      //static constexpr bool TypeErased    = true;
      //static constexpr bool DeeplyOwned   = true;
      //static constexpr bool HeapCanBeNull = true;

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
         this->Com::HeapReference<HeapEntry<0>>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<HeapEntry<1>>::SetHeapInner(val.GetHeapInner());
         this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>::SetEntriesInner(key.GetEntriesInner());
         this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>::SetEntriesInner(val.GetEntriesInner());
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(key.GetTypeInner());
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(val.GetTypeInner());
      }

      /// Assignment is disabled                                              
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      Handle GetKey() noexcept {
         return {
            this->Com::HeapReference<HeapEntry<0>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 0>::GetTypeInner()
         };
      }
      Handle GetKeyHandle() noexcept {
         return GetKey();
      }

      HandleMut GetVal() noexcept {
         return {
            this->Com::HeapReference<HeapEntry<1>>::GetHeapInner(),
            this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 1>::GetTypeInner()
         };
      }
      HandleMut GetValHandle() noexcept {
         return GetVal();
      }

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      decltype(auto) ForceMutable(this auto&& self) noexcept {
         return reinterpret_cast<THandlePair<HandleMut, HandleMut>&&>(LglsFwd(self));
      }

      /// Pick a specific dimension                                           
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
              if constexpr (SID == 0)  return self.GetKeyHandle();
         else if constexpr (SID == 1)  return self.GetValHandle();
         else static_assert(false, "No such dimension");
      }
   };


   ///                                                                        
   /// Statically typed emergent handles                                      
   ///                                                                        
   template<CT::Reference K, CT::Reference V> requires CT::NotSheddable<K, V>
   struct THandlePair<THandleEmergent<K>, THandleEmergent<V>> : Com::Container<
      Com::Multitype<Com::TypedStatic<DMeta, Deref<K>, 0>,
                     Com::TypedStatic<DMeta, Deref<V>, 1>>,
      Com::Multiprovider<Com::HeapReference<HeapEntry<0, Deref<K>*>>,
                         Com::HeapReference<HeapEntry<1, Deref<V>*>>>,
      Com::CountStatic<1u, 0, 1>,
      //Com::Multiown    <EnableComponentIf<CT::Dense<K>,  Com::OwnershipEmergent<Com::WeakOwnership, 0>>,
      //                  EnableComponentIf<CT::Dense<V>,  Com::OwnershipEmergent<Com::WeakOwnership, 1>>>,
      Com::MultiownDeep<EnableComponentIf<CT::Sparse<K>, Com::OwnershipDeepEmergent<Com::WeakOwnership, true, 0>>,
                        EnableComponentIf<CT::Sparse<V>, Com::OwnershipDeepEmergent<Com::WeakOwnership, true, 1>>>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Comparison<true, 0, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;
      //using CTTI_Typed     = Types<Deref<K>, Deref<V>>;

      using Denser    = THandlePair<typename THandleEmergent<K>::Denser, typename THandleEmergent<V>::Denser>;
      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = THandleEmergent<K>;
      using ValHandle = THandleEmergent<V>;

      //static constexpr bool Emergent      = true;
      //static constexpr bool HeapCanBeNull = true;
      //static constexpr uint Owned         = CT::Dense<K> or CT::Dense<V> ? Com::WeakOwnership : 0;
      //static constexpr bool DeeplyOwned   = CT::Sparse<K> or CT::Sparse<V>;

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

      constexpr THandlePair(CT::Handle auto&& key, CT::Handle auto&& val) noexcept {
         this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::SetHeapInner(DeintCast(key).GetHeapInner());
         this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::SetHeapInner(DeintCast(val).GetHeapInner());
      }

      /// Assignment is disabled                                              
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      decltype(auto) GetKey() noexcept {
         if constexpr (CT::Constant<K>)
            return *this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::Get();
         else
            return GetKeyHandle();
      }
      KeyHandle GetKeyHandle() noexcept {
         return THandleEmergent<K> {
            this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::GetHeapInner()
         };
      }

      decltype(auto) GetVal() noexcept {
         if constexpr (CT::Constant<V>)
            return *this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::Get();
         else
            return GetValHandle();
      }
      ValHandle GetValHandle() noexcept {
         return THandleEmergent<V> {
            this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::GetHeapInner()
         };
      }

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      decltype(auto) ForceMutable(this auto&& self) noexcept {
         return reinterpret_cast<THandlePair<
            THandleEmergent<Decvq<Deref<K>>&>,
            THandleEmergent<Decvq<Deref<V>>&>
         >&&>(LglsFwd(self));
      }

      /// Pick a specific dimension                                           
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
              if constexpr (SID == 0) return self.GetKeyHandle();
         else if constexpr (SID == 1) return self.GetValHandle();
         else static_assert(false, "No such dimension");
      }
   };


   ///                                                                        
   /// Statically typed embedded handles                                      
   ///                                                                        
   template<CT::Reference K, CT::Reference V> requires CT::NotSheddable<K, V>
   struct THandlePair<THandle<K>, THandle<V>> : Com::Container<
      Com::Multitype<Com::TypedStatic<DMeta, Deref<K>, 0>,
                     Com::TypedStatic<DMeta, Deref<V>, 1>>,
      Com::Multiprovider<Com::HeapReference<HeapEntry<0, Deref<K>*>>,
                         Com::HeapReference<HeapEntry<1, Deref<V>*>>>,
      Com::CountStatic<1u, 0, 1>,
      //Com::Multiown    <EnableComponentIf<CT::Dense<K>,  Com::OwnershipStack<Com::WeakOwnership, 0>>,
      //                  EnableComponentIf<CT::Dense<V>,  Com::OwnershipStack<Com::WeakOwnership, 1>>>,
      Com::MultiownDeep<EnableComponentIf<CT::Sparse<K>, Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>>,
                        EnableComponentIf<CT::Sparse<V>, Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>>>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Assignment<0, 1>,
      Com::Emplacement<0, 1>,
      Com::Comparison<true, 0, 1>,
      Com::IterationOperators<0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;
      //using CTTI_Typed     = Types<Deref<K>, Deref<V>>;

      using Denser    = THandlePair<typename THandle<K>::Denser, typename THandle<V>::Denser>;
      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = THandle<K>;
      using ValHandle = THandle<V>;

      //static constexpr bool TypeErased        = false;
      //static constexpr uint Owned             = CT::Dense<K> or CT::Dense<V> ? Com::WeakOwnership : 0;
      //static constexpr bool DeeplyOwned       = CT::Sparse<K> or CT::Sparse<V>;
      static constexpr bool ReferenceElements = true;
      //static constexpr bool HeapCanBeNull     = true;

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

         /*if constexpr (CT::Dense<K>)
            this->Com::OwnershipStack<Com::WeakOwnership, 0>::SetAllocationInner(key.GetAllocation());
         else*/
         if constexpr (CT::Sparse<K>)
            this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>::SetEntriesInner(key.GetEntries());

         /*if constexpr (CT::Dense<V>)
            this->Com::OwnershipStack<Com::WeakOwnership, 1>::SetAllocationInner(val.GetAllocation());
         else*/
         if constexpr (CT::Sparse<V>)
            this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>::SetEntriesInner(val.GetEntries());
      }

      /// Assignment is disabled                                              
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      decltype(auto) GetKey() noexcept {
         if constexpr (CT::Constant<K>)
            return *this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::Get();
         else
            return GetKeyHandle();
      }

      KeyHandle GetKeyHandle() noexcept {
         /*if constexpr (CT::Dense<K, V>) {
            return KeyHandle {
               this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::GetHeapInner(),
               this->Com::OwnershipStack<Com::WeakOwnership, 0, 1>::GetAllocation()
            };
         }
         else if constexpr (CT::Dense<K>) {
            return KeyHandle {
               this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::GetHeapInner(),
               this->Com::OwnershipStack<Com::WeakOwnership, 0>::GetAllocation()
            };
         }*/
         if constexpr (CT::Sparse<K>) {
            return KeyHandle {
               this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::GetHeapInner(),
               this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 0>::GetEntriesInner()
            };
         }
         else {
            return KeyHandle {
               this->Com::HeapReference<HeapEntry<0, Deref<K>*>>::GetHeapInner()
            };
         }
      }

      decltype(auto) GetVal() noexcept {
         if constexpr (CT::Constant<V>)
            return *this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::Get();
         else
            return GetValHandle();
      }

      ValHandle GetValHandle() noexcept {
         /*if constexpr (CT::Dense<K, V>) {
            return ValHandle {
               this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::GetHeapInner(),
               this->Com::OwnershipStack<Com::WeakOwnership, 0, 1>::GetAllocation()
            };
         }
         else if constexpr (CT::Dense<V>) {
            return ValHandle {
               this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::GetHeapInner(),
               this->Com::OwnershipStack<Com::WeakOwnership, 1>::GetAllocation()
            };
         }*/
         if constexpr (CT::Sparse<V>) {
            return ValHandle {
               this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::GetHeapInner(),
               this->Com::OwnershipDeepReference<Com::WeakOwnership, true, 1>::GetEntriesInner()
            };
         }
         else {
            return ValHandle {
               this->Com::HeapReference<HeapEntry<1, Deref<V>*>>::GetHeapInner()
            };
         }
      }

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      decltype(auto) ForceMutable(this auto&& self) noexcept {
         return reinterpret_cast<THandlePair<
            THandle<Decvq<Deref<K>>&>,
            THandle<Decvq<Deref<V>>&>
         >&&>(LglsFwd(self));
      }

      /// Pick a specific dimension                                           
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
              if constexpr (SID == 0)  return self.GetKeyHandle();
         else if constexpr (SID == 1)  return self.GetValHandle();
         else static_assert(false, "No such dimension");
      }
   };


   ///                                                                        
   /// Statically typed local handles                                         
   ///                                                                        
   template<CT::NotReference K, CT::NotReference V> requires (CT::NotSheddable<K, V> and CT::NotHandle<K, V>)
   struct THandlePair<THandle<K>, THandle<V>> : Com::Container<
      Com::Multitype<Com::TypedStatic<DMeta, Deref<K>, 0>,
                     Com::TypedStatic<DMeta, Deref<V>, 1>>,
      Com::Multiprovider<EnableComponentIf<CT::Dense<K>,  Com::Stack<K, 0>>,
                         EnableComponentIf<CT::Dense<V>,  Com::Stack<V, 1>>,
                         EnableComponentIf<CT::Sparse<K>, Com::HeapMovable<0, 0, HeapEntry<0, K*>>>,
                         EnableComponentIf<CT::Sparse<V>, Com::HeapMovable<0, 0, HeapEntry<1, V*>>>>,
      Com::CountStatic<1u, 0, 1>,
      Com::ReserveStatic<1u, 0, 1>,
      //EnableComponentIf<CT::Sparse<K>, Com::ReserveEmergent<size_t, 0>>,
      //EnableComponentIf<CT::Sparse<V>, Com::ReserveEmergent<size_t, 1>>,
      Com::Multiown    <EnableComponentIf<CT::Sparse<K>, Com::OwnershipStack<Com::StrongOwnership, 0>>,
                        EnableComponentIf<CT::Sparse<V>, Com::OwnershipStack<Com::StrongOwnership, 1>>>,
      Com::MultiownDeep<EnableComponentIf<CT::Sparse<K>, Com::OwnershipDeepHeap<Com::StrongOwnership, true, 0>>,
                        EnableComponentIf<CT::Sparse<V>, Com::OwnershipDeepHeap<Com::StrongOwnership, true, 1>>>,
      Com::HashEmergent<0, Hash, 1>,
      Com::Assignment<0, 1>,
      Com::Emplacement<0, 1>,
      Com::Comparison<true, 0, 1>
   > {
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_ReflectAs = void;
      //using CTTI_Typed     = Types<Deref<K>, Deref<V>>;

      using Denser    = THandlePair<typename THandle<K>::Denser, typename THandle<V>::Denser>;
      using DeepType  = HandleDisowned; // TODO why disowned?
      using KeyHandle = Tif<CT::Sparse<K>, THandle<K&>, THandleEmergent<K&>>;
      using ValHandle = Tif<CT::Sparse<V>, THandle<V&>, THandleEmergent<V&>>;

      //static constexpr bool TypeErased        = false;
      //static constexpr uint Owned             = CT::Sparse<K> or CT::Sparse<V> ? Com::StrongOwnership : 0;
      //static constexpr bool DeeplyOwned       = CT::Sparse<K> or CT::Sparse<V>;
      static constexpr bool ReferenceElements = true;
      //static constexpr bool HeapCanBeNull     = DeeplyOwned;

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
         Com::Emplacement<0, 1>::template EmplaceConstruct<0>(LglsFwd(key));
         Com::Emplacement<0, 1>::template EmplaceConstruct<1>(LglsFwd(val));
      }

      constexpr ~THandlePair() noexcept {
         this->Destroy();
      }

      /// Assignment is disabled                                              
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      auto GetKey() noexcept -> KeyHandle {
         if constexpr (CT::Sparse<K>) {
            return THandle<K&> {
               this->Com::HeapMovable<0, 0, HeapEntry<0, K*>>::GetRaw(),
               this->Com::OwnershipDeepHeap<Com::StrongOwnership, true, 0>::GetEntriesInner()
            };
         }
         else return THandleEmergent<K&> {this->Com::Stack<K, 0>::GetRaw()};
      }

      auto GetKeyHandle() noexcept -> KeyHandle {
         return GetKey();
      }

      auto GetVal() noexcept -> ValHandle {
         if constexpr (CT::Sparse<V>) {
            return THandle<V&> {
               this->Com::HeapMovable<0, 0, HeapEntry<1, V*>>::GetRaw(),
               this->Com::OwnershipDeepHeap<Com::StrongOwnership, true, 1>::GetEntriesInner()
            };
         }
         else return THandleEmergent<V&> {this->Com::Stack<V, 1>::GetRaw()};
      }

      auto GetValHandle() noexcept -> ValHandle {
         return GetVal();
      }

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      decltype(auto) ForceMutable(this auto&& self) noexcept {
         return reinterpret_cast<THandlePair<THandle<Decvq<K>>, THandle<Decvq<V>>>&&>(self);
      }

      /// Pick a specific dimension                                           
      template<Cid SID>
      constexpr decltype(auto) PickDimension(this auto&& self) noexcept {
              if constexpr (SID == 0)  return self.GetKeyHandle();
         else if constexpr (SID == 1)  return self.GetValHandle();
         else static_assert(false, "No such dimension");
      }
   };

   template<CT::Handle K, CT::Handle V>
   THandlePair(K&&, V&&) -> THandlePair<Decay<K>, Decay<V>>;
}
