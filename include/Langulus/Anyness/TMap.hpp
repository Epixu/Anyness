///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Map.hpp"


namespace Langulus::Anyness::Inner
{
   template<CT::NotVoid K, CT::NotVoid V, StateValue SORT>
   requires (CT::NotHandle<K, V> and CT::NotReference<K, V>)
   using TMapBase = Com::Container<
      Com::TypedStack<DMeta, K, true, 0>, // Type-constrained keys      
      Com::TypedStack<DMeta, V, true, 1>, // Type-constrained values    
      Com::HeapMovable<8, 2,
         HeapEntry<0, K*>,                // Key heap data              
         HeapEntry<1, V*>                 // Value heap data            
      >,
      Com::CountStack<0, size_t, 1>,      // Dynamically sized          
      Com::ReserveStack<size_t, 0, 1>,    // Reserve kept as member     
      Com::IndexedHashStack<0, Hash, 1>,  // Indexed by hash table      
      Com::OwnershipStack<0, Com::StrongOwnership, 1>,
      Com::OwnershipDeepHeap<0, true, 1>, // Separate key deep ownership
      Com::HashHeap<0, Hash, 1>,          // Hash can be cached         
      Com::Merging<0, void, 1>,           // Only merging for keys      
      //Com::Insertion<1>,                  // Allows inserting values    
      //Com::Assignment<1>,                 // Allows assignment of values
      Com::Removal<0, 1>,                 // Allows clear/reset of K/V  
      Com::Conversion<0, 1>,              // Allows conversions of K/V  
      Com::Comparison<0, true, 1>,        // Allows comparisons of K/V  
      Com::IterationForEach<0, 1>,        // ForEach iteration of K/V   
      Com::IterationRange<0, 1>,          // Ranged iteration of K/V    
      Com::State::Sorted<SORT>,           // Toggle ordered map         
      Com::State::Compressed<>,           // Toggle compression         
      Com::State::Encrypted<>             // Toggle encryption          
   >;
}

namespace Langulus::Anyness
{
   ///                                                                        
   /// A statically-typed non-contiguous map of variable size that is         
   /// binary-compatible with the type-erased alternative `Map`.              
   /// Emplacement is disabled for maps, because keys aren't allowed to       
   /// change in-place. This also means that they are only const-iteratable.  
   /// Values, on the other hand, are mutable.                                
   template<CT::NotVoid K, CT::NotVoid V, StateValue SORT>
   struct TMap : Inner::TMapBase<K, V, SORT> {
      using CTTI_ReflectAs = Map;
      using CTTI_Map       = Yes<>;
      using CTTI_Deep      = Yes<>;
      using CTTI_MapsTo    = Text;
      using CTTI_Typed     = Types<K, V>;

      using Base           = Inner::TMapBase<K, V, SORT>;
      using DeepType       = Any;

      using HandleType     = THandlePair<THandle<ConstAll<K&>>, THandle<ConstAll<V&>>>;
      using HandleMutType  = THandlePair<THandle<ConstAll<K&>>, THandle<V&>>;
      using Pick           = HandleType;
      using PickMut        = HandleMutType;

      static constexpr bool TypeErased = false;
      static constexpr bool DeeplyOwned = true;
      static constexpr bool ReferenceElements = true;

      using Key = K;
      using Val = V;

      constexpr TMap() noexcept {
         this->ConstructDefault();
      }
      constexpr TMap(TMap const& other) {
         this->Absorb(Refer(other));
      }
      constexpr TMap(TMap&& other) noexcept {
         this->Absorb(Move(other));
      }
      constexpr ~TMap() noexcept {
         this->Destroy();
      }
      
      /// Construction that either absorbs the provided containers, or        
      /// emplaces all A in the container                                     
      template<class A1, class...AN>
      constexpr TMap(A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0 and CT::Map<A1>)
            this->Absorb(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Merge(LglsFwd(a1));
           (this->Merge(LglsFwd(an)), ...);
         }
      }
      
      /// Construction that absorbs the provided container                    
      template<class A1, class...AN>
      constexpr TMap(Inner::Absorb, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->Absorb(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->MergeRange(LglsFwd(a1));
           (this->MergeRange(LglsFwd(an)), ...);
         }
      }
      
      /// Construction that emplaces all arguments inside                     
      template<class A1, class...AN>
      constexpr TMap(Inner::Piecewise, A1&& a1, AN&&...an) {
         this->ConstructDefault();
         this->Merge(LglsFwd(a1));
        (this->Merge(LglsFwd(an)), ...);
      }

      /// Assignment                                                          
      constexpr TMap& operator = (TMap const& other) {
         return this->AssignAbsorb(Refer(other));
      }
      constexpr TMap& operator = (TMap&& other) noexcept {
         return this->AssignAbsorb(Move(other));
      }

      template<class A>
      constexpr TMap& operator = (A&& argument) {
         if constexpr (CT::Map<A>)
            return this->AssignAbsorb(LglsFwd(argument));
         else
            return this->Assign(LglsFwd(argument));
      }

      /// Clear the map and assign a single pair                              
      auto Assign(CT::Pair auto&& pair) -> TMap& {
         using I = IntentOf(pair);
         this->Clear();
         this->MergeInner(I::Nest(pair.GetKey()), I::Nest(pair.GetVal()));
         return *this;
      }

      /// Clear the map and assign a key and a value                          
      auto Assign(auto&& key, auto&& val) -> TMap& {
         this->Clear();
         this->MergeInner(LglsFwd(key), LglsFwd(val));
         return *this;
      }

      using Com::Comparison<0, true, 1>::operator <=>;
      using Com::Comparison<0, true, 1>::operator ==;

      constexpr bool IsKeyTyped() const noexcept {
         return true;
      }
      constexpr bool IsValTyped() const noexcept {
         return true;
      }

      constexpr bool IsKeySparse() const noexcept {
         return CT::Sparse<K>;
      }
      constexpr bool IsValSparse() const noexcept {
         return CT::Sparse<V>;
      }

      constexpr bool IsKeyDeep() const noexcept {
         return this->template IsDeep<0>();
      }
      constexpr bool IsValDeep() const noexcept {
         return this->template IsDeep<1>();
      }

      constexpr bool IsKeyTypeConstrained() const noexcept {
         return this->template IsTypeConstrained<0>();
      }
      constexpr bool IsValTypeConstrained() const noexcept {
         return this->template IsTypeConstrained<1>();
      }

      constexpr bool IsKeyConstant() const noexcept {
         return true;
      }
      constexpr bool IsValConstant() const noexcept {
         return this->template IsConstant<1>();
      }

      template<class T>
      constexpr bool IsKey() const noexcept {
         return this->template Is<T, 0>();
      }
      template<class T>
      constexpr bool IsVal() const noexcept {
         return this->template Is<T, 1>();
      }
      template<class T>
      constexpr bool IsKeySame() const noexcept {
         return this->template IsSame<T, 0>();
      }
      template<class T>
      constexpr bool IsValSame() const noexcept {
         return this->template IsSame<T, 1>();
      }
      template<class T>
      constexpr bool IsKeyExact() const noexcept {
         return this->template IsExact<T, 0>();
      }
      template<class T>
      constexpr bool IsValExact() const noexcept {
         return this->template IsExact<T, 1>();
      }

      constexpr bool IsKey(DMeta type) const noexcept {
         return this->template Is<0>(type);
      }
      constexpr bool IsVal(DMeta type) const noexcept {
         return this->template Is<1>(type);
      }
      constexpr bool IsKeySame(DMeta type) const noexcept {
         return this->template IsSame<0>(type);
      }
      constexpr bool IsValSame(DMeta type) const noexcept {
         return this->template IsSame<1>(type);
      }
      constexpr bool IsKeyExact(DMeta type) const noexcept {
         return this->template IsExact<0>(type);
      }
      constexpr bool IsValExact(DMeta type) const noexcept {
         return this->template IsExact<1>(type);
      }

      constexpr DMeta GetKeyType() const noexcept {
         return this->template GetType<0>();
      }
      constexpr DMeta GetValType() const noexcept {
         return this->template GetType<1>();
      }

      template<CT::NotVoid AS>
      decltype(auto) KeyAsAt(this auto&& self, CT::Index auto&& idx) {
         return self.template AsAt<AS, 0>(LglsFwd(idx));
      }
      template<CT::NotVoid AS>
      decltype(auto) ValAsAt(this auto&& self, CT::Index auto&& idx) {
         return self.template AsAt<AS, 1>(LglsFwd(idx));
      }

      constexpr auto GetKeyEntries() const noexcept {
         return this->template GetEntries<0>();
      }
      constexpr auto GetValEntries() const noexcept {
         return this->template GetEntries<1>();
      }

      auto GetKeyEntriesAt(CT::Index auto&& idx) const assumptious {
         return this->template GetEntriesAt<0>(LglsFwd(idx));
      }
      auto GetValEntriesAt(CT::Index auto&& idx) const assumptious {
         return this->template GetEntriesAt<1>(LglsFwd(idx));
      }
   };

   template<CT::NotVoid K, CT::NotVoid V>
   using TMapSorted = TMap<K, V, StateValue::Enabled>;

   template<CT::NotVoid K, CT::NotVoid V>
   using TMapUnsorted = TMap<K, V, StateValue::Disabled>;
}

namespace Langulus::CTTI
{
   /// Convert TMap -> Text                                                   
   template<CT::NotVoid K, CT::NotVoid V, Anyness::StateValue SORT>
   struct Converter<Anyness::TMap<K, V, SORT>, Anyness::Text> {
      static constexpr auto Convert(Anyness::TMap<K, V, SORT> const&) -> Anyness::Text;
   };
}
