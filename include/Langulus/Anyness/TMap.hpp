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
   template<CT::NotVoid K, CT::NotVoid V, State::StateValue SORT>
   using TMapBase = Com::Container<
      Com::TypedStack<DMeta, K, true, 0>, // Type-constrained keys      
      Com::TypedStack<DMeta, V, true, 1>, // Type-constrained values    
      Com::HeapMovable<0, 8, 2,
         HeapEntry<0, K*>,                // Key heap data              
         HeapEntry<1, V*>                 // Value heap data            
      >,
      Com::CountStack<0, size_t, 1>,      // Dynamically sized          
      Com::ReserveStack<0, size_t, 1>,    // Reserve kept as member     
      Com::IndexedHashStack<0, Hash, 1>,  // Indexed by hash table      
      Com::OwnershipStack<0, Com::StrongOwnership, 1>,
      Com::OwnershipDeepHeap<0>,          // Separate key deep ownership
      Com::OwnershipDeepHeap<1>,          // Separate val deep onwership
      Com::HashHeap<0, Hash, 1>,          // Hash can be cached         
      Com::Merging<0, void, 1>,           // Only merging for keys      
      Com::Insertion<1>,                  // Allows inserting values    
      Com::Assignment<1>,                 // Allows assignment of values
      Com::Removal<0, 1>,                 // Allows clear/reset of K/V  
      Com::Conversion<0, 1>,              // Allows conversions of K/V  
      Com::Comparison<0, true, 1>,        // Allows comparisons of K/V  
      Com::IterationForEach<0, 1>,        // ForEach iteration of K/V   
      Com::IterationRange<0, 1>,          // Ranged iteration of K/V    
      Com::StateStack<                    // Variable state             
         DefineState::Typed<State::Enabled>, // Always type-constrained 
         DefineState::Sorted<SORT>,       // Maybe unsorted             
         DefineState::Compressed<>,       // Adds 'compressed' state    
         DefineState::Encrypted<>,        // Adds 'encrypted' state     
         DefineState::Tracked<>           // Adds 'tracked' state       
      >
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
   template<CT::NotVoid K, CT::NotVoid V, State::StateValue SORT>
   struct TMap : Inner::TMapBase<K, V, SORT> {
      using CTTI_Map       = Yes<>;
      using CTTI_ReflectAs = Map;
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
         if constexpr (sizeof...(AN) == 0) {
            if constexpr (SameAsOneOf<Deint<A1>, 
               TMap<K, V, State::Variable>, TMap<K, V, State::Enabled>, TMap<K, V, State::Disabled>,
               Map, MapSorted, MapUnsorted>
            ) {
               LglsAssumeUser((not SameAsOneOf<K,
                  TMap<K, V, State::Variable>, TMap<K, V, State::Enabled>, TMap<K, V, State::Disabled>,
                  Map, MapSorted, MapUnsorted>),
                  "Ambiguous use of construction "
                  "- you should use tag-dispatch with first argument either Absorb "
                  "(if you want to overwrite the container itself) or Piecewise "
                  "(if you want to overwrite the first item) in order to clearly "
                  "state your intent. Absorb will be used by default!"
               );
               this->Absorb(LglsFwd(a1));
            }
            else {
               this->ConstructDefault();
               this->Merge(LglsFwd(a1));
            }
         }
         else {
            this->ConstructDefault();
            this->Insert(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that absorbs the provided container                    
      template<class A1, class...AN>
      constexpr TMap(Inner::Absorb, A1&& a1, AN&&...an) {
         if constexpr (sizeof...(AN) == 0)
            this->Absorb(LglsFwd(a1));
         else {
            this->ConstructDefault();
            this->Concat(LglsFwd(a1), LglsFwd(an)...);
         }
      }
      
      /// Construction that emplaces all arguments inside                     
      template<class A1, class...AN>
      constexpr TMap(Inner::Piecewise, A1&& a1, AN&&...an) {
         this->ConstructDefault();
         this->Merge(LglsFwd(a1), LglsFwd(an)...);
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
         if constexpr (SameAsOneOf<Deint<A>,
            TMap<K, V, State::Variable>, TMap<K, V, State::Enabled>, TMap<K, V, State::Disabled>,
            Map, MapSorted, MapUnsorted>
         ) {
            LglsAssumeUser((not SameAsOneOf<K,
               TMap<K, V, State::Variable>, TMap<K, V, State::Enabled>, TMap<K, V, State::Disabled>,
               Map, MapSorted, MapUnsorted>),
               "Ambiguous use of assignment "
               "- you should use either AssignAbsorb (if you want to overwrite "
               "the container itself) or Assign (if you want to overwrite the "
               "first item) in order to clearly state your intent. "
               "AssignAbsorb will be used by default!"
            );
            return this->AssignAbsorb(LglsFwd(argument));
         }
         else return this->Assign(LglsFwd(argument));
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
      decltype(auto) KeyAsAt(CT::Index auto&& idx) {
         return this->template AsAt<AS, 0>(LglsFwd(idx));
      }
      template<CT::NotVoid AS>
      decltype(auto) ValAsAt(CT::Index auto&& idx) {
         return this->template AsAt<AS, 1>(LglsFwd(idx));
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
   using TMapSorted = TMap<K, V, State::Enabled>;

   template<CT::NotVoid K, CT::NotVoid V>
   using TMapUnsorted = TMap<K, V, State::Disabled>;
}

namespace Langulus::CTTI
{
   /// Convert TMap -> Text                                                   
   template<CT::NotVoid K, CT::NotVoid V, Anyness::State::StateValue SORT>
   struct Converter<Anyness::TMap<K, V, SORT>, Anyness::Text> {
      static constexpr auto Convert(Anyness::TMap<K, V, SORT> const&) -> Anyness::Text;
   };
}
