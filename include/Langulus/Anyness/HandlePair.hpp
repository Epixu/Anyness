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
      Com::HeapReference<0>,
      Com::HeapReference<1>,
      Com::CountStatic<0, 1u, 1>,
      Com::OwnershipDeepReference<0>,
      Com::OwnershipDeepReference<1>,
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

      /*constexpr THandlePair(
         void const* ptr0, EntryPtr entry0, DMeta type0,
         void const* ptr1, EntryPtr entry1, DMeta type1
      ) noexcept {
         this->Com::HeapReference<0>::SetHeapInner(ptr0);
         this->Com::HeapReference<1>::SetHeapInner(ptr1);
         this->Com::OwnershipDeepReference<0>::SetEntriesInner(entry0);
         this->Com::OwnershipDeepReference<1>::SetEntriesInner(entry1);
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(type0);
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(type1);
      }*/

      constexpr THandlePair(Handle&& key, Handle&& val) noexcept {
         this->Com::HeapReference<0>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<1>::SetHeapInner(val.GetHeapInner());
         this->Com::OwnershipDeepReference<0>::SetEntriesInner(key.GetEntriesInner());
         this->Com::OwnershipDeepReference<1>::SetEntriesInner(val.GetEntriesInner());
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(key.GetTypeInner());
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(val.GetTypeInner());
      }

      /// Assignment                                                          
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      Handle GetKey() {
         return {
            this->Com::HeapReference<0>::GetHeapInner(),
            this->Com::OwnershipDeepReference<0>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 0>::GetTypeInner()
         };
      }

      Handle GetVal() {
         return {
            this->Com::HeapReference<1>::GetHeapInner(),
            this->Com::OwnershipDeepReference<1>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 1>::GetTypeInner()
         };
      }

   protected:
      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() const noexcept -> THandlePair<HandleMut, HandleMut> const& {
         return *reinterpret_cast<THandlePair<HandleMut, HandleMut> const*>(this);
      }
   };
   
   /// Type-erased mutable handle                                             
   template<>
   struct THandlePair<HandleMut, HandleMut> : Com::Container<
      Com::TypedStack<DMeta, void, false, 0>,
      Com::TypedStack<DMeta, void, false, 1>,
      Com::HeapReference<0>,
      Com::HeapReference<1>,
      Com::CountStatic<0, 1u, 1>,
      Com::OwnershipDeepReference<0>,
      Com::OwnershipDeepReference<1>,
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

      /*constexpr THandlePair(
         void const* ptr0, EntryPtr entry0, DMeta type0,
         void const* ptr1, EntryPtr entry1, DMeta type1
      ) noexcept {
         this->Com::HeapReference<0>::SetHeapInner(ptr0);
         this->Com::HeapReference<1>::SetHeapInner(ptr1);
         this->Com::OwnershipDeepReference<0>::SetEntriesInner(entry0);
         this->Com::OwnershipDeepReference<1>::SetEntriesInner(entry1);
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(type0);
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(type1);
      }*/

      constexpr THandlePair(HandleMut&& key, HandleMut&& val) noexcept {
         this->Com::HeapReference<0>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<1>::SetHeapInner(val.GetHeapInner());
         this->Com::OwnershipDeepReference<0>::SetEntriesInner(key.GetEntriesInner());
         this->Com::OwnershipDeepReference<1>::SetEntriesInner(val.GetEntriesInner());
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(key.GetTypeInner());
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(val.GetTypeInner());
      }

      /// Assignment                                                          
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      HandleMut GetKey() {
         return {
            this->Com::HeapReference<0>::GetHeapInner(),
            this->Com::OwnershipDeepReference<0>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 0>::GetTypeInner()
         };
      }

      HandleMut GetVal() {
         return {
            this->Com::HeapReference<1>::GetHeapInner(),
            this->Com::OwnershipDeepReference<1>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 1>::GetTypeInner()
         };
      }

      /// Already as mutable as it gets                                       
      auto ForceMutable() const noexcept -> THandlePair const& {
         return *this;
      }
   };
   
   /// Type-erased immutable key paired with mutable value                    
   /// Often used for mutable access in maps, where keys can't be modified    
   template<>
   struct THandlePair<Handle, HandleMut> : Com::Container<
      Com::TypedStack<DMeta, void, false, 0>,
      Com::TypedStack<DMeta, void, false, 1>,
      Com::HeapReference<0>,
      Com::HeapReference<1>,
      Com::CountStatic<0, 1u, 1>,
      Com::OwnershipDeepReference<0>,
      Com::OwnershipDeepReference<1>,
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

      /*constexpr THandlePair(
         void const* ptr0, EntryPtr entry0, DMeta type0,
         void const* ptr1, EntryPtr entry1, DMeta type1
      ) noexcept {
         this->Com::HeapReference<0>::SetHeapInner(ptr0);
         this->Com::HeapReference<1>::SetHeapInner(ptr1);
         this->Com::OwnershipDeepReference<0>::SetEntriesInner(entry0);
         this->Com::OwnershipDeepReference<1>::SetEntriesInner(entry1);
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(type0);
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(type1);
      }*/

      constexpr THandlePair(Handle&& key, HandleMut&& val) noexcept {
         this->Com::HeapReference<0>::SetHeapInner(key.GetHeapInner());
         this->Com::HeapReference<1>::SetHeapInner(val.GetHeapInner());
         this->Com::OwnershipDeepReference<0>::SetEntriesInner(key.GetEntriesInner());
         this->Com::OwnershipDeepReference<1>::SetEntriesInner(val.GetEntriesInner());
         this->Com::TypedStack<DMeta, void, false, 0>::SetTypeInner(key.GetTypeInner());
         this->Com::TypedStack<DMeta, void, false, 1>::SetTypeInner(val.GetTypeInner());
      }

      /// Assignment                                                          
      THandlePair& operator = (THandlePair const& other) = delete;
      THandlePair& operator = (THandlePair&& other) = delete;

      Handle GetKey() {
         return {
            this->Com::HeapReference<0>::GetHeapInner(),
            this->Com::OwnershipDeepReference<0>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 0>::GetTypeInner()
         };
      }

      HandleMut GetVal() {
         return {
            this->Com::HeapReference<1>::GetHeapInner(),
            this->Com::OwnershipDeepReference<1>::GetEntriesInner(),
            this->Com::TypedStack<DMeta, void, false, 1>::GetTypeInner()
         };
      }

      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() const noexcept -> THandlePair<HandleMut, HandleMut> const& {
         return *reinterpret_cast<THandlePair<HandleMut, HandleMut> const*>(this);
      }
   };


   ///                                                                        
   /// Pair of handles                                                        
   //TODO this is a temporary setup. A better one would probably be to       
   // concatenate the components of the two handles, offsetting the IDs of   
   // V, and thus composing a new container to represent the pair.           
   /*template<CT::Handle K, CT::Handle V> 
   struct THandlePair {
      using CTTI_Container = Yes<>;
      using CTTI_Deep      = Yes<>;
      using CTTI_Handle    = Yes<>;
      using CTTI_Pair      = Yes<>;
      using CTTI_Typed     = Types<TypeOf<K>, TypeOf<V>>;
      using CTTI_ReflectAs = void;

      static constexpr bool ContainsMany = false;
      using Dimensions = Values<0, 1>;

      //using Denser         = Types<typename K::Denser,   typename V::Denser>;
      //using DeepType       = Types<typename K::DeepType, typename V::DeepType>;

      using KeyHandleType = K;
      using ValHandleType = V;

      static_assert(CT::NoIntent<K, V> and CT::Decayed<K, V>);

      K key;
      V val;

      constexpr explicit operator bool() const noexcept {
         return static_cast<bool>(key);
      }

      auto& GetKey(this auto&& self) noexcept {
         return self.key;
      }
      auto& GetVal(this auto&& self) noexcept {
         return self.val;
      }

      /// Get raw data associated with the key                                
      auto GetRaw() const noexcept {
         return key.GetRaw();
      }

      /// Get the hash of the pair                                            
      auto GetHash() const -> Hash {
         return key.GetHash() ^ val.GetHash();
      }
      
      /// Force the handle to become mutable, so that we have methods like    
      /// emplacement in constructors.                                        
      auto ForceMutable() const noexcept {
         return THandlePair<
            Decay<decltype(key.ForceMutable())>,
            Decay<decltype(val.ForceMutable())>
         > {key.ForceMutable(), val.ForceMutable()};
      }*/

      /*void SwapInner(CT::ContainsOne auto& rhs) {
         key.SwapInner(LglsFwd(rhs));
         val.SwapInner(LglsFwd(rhs));
      }*/

      /*template<CT::Pair P> requires CT::NoIntent<P>
      void SwapInner(P& rhs) {
         key.SwapInner(rhs.GetKey());
         val.SwapInner(rhs.GetVal());
      }

      template<CT::Intent I> requires CT::Pair<I>
      void EmplaceWithIntent(I&& intent) {
         key.EmplaceWithIntent(I::Nest(intent->GetKey()));
         val.EmplaceWithIntent(I::Nest(intent->GetVal()));
      }

      template<bool DESTROY = true>
      void DestroyElement() {
         key.template DestroyElement<DESTROY>();
         val.template DestroyElement<DESTROY>();
      }

      template<bool FIND_MISSING = false>
      void KeepElementDeepCustomPointers() {
         key.template KeepElementDeepCustomPointers<FIND_MISSING>();
         val.template KeepElementDeepCustomPointers<FIND_MISSING>();
      }

      /// Offset pair to the right by the desired amount                      
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return a shallow modified copy of this container                 
      constexpr auto operator + (size_t offset) const assumptious -> THandlePair {
         THandlePair copy = *this;
         return copy += offset;
      }

      /// Offset pair element to the right by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return reference to this, after being modified                   
      constexpr auto operator += (size_t offset) assumptious -> THandlePair& {
         key += offset;
         val += offset;
         return *this;
      }

      /// Prefix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      constexpr auto operator ++ () assumptious -> THandlePair& {
         return (*this += 1);
      }

      /// Suffix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      constexpr auto operator ++ (int) assumptious -> THandlePair {
         THandlePair backup = *this;
         *this += 1;
         return backup;
      }
      
      /// Get the element difference between two iterators                    
      ///   @attention very usafe - assumes rhs's type is same as self        
      ///   @param rhs the other iterator                                     
      ///   @return the difference in number of elements                      
      constexpr auto operator - (THandlePair const& rhs) const assumptious -> ::std::ptrdiff_t {
         return key - rhs.key;
      }
      
      /// Offset first element to the left by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return a shallow modified copy of this container                 
      constexpr auto operator - (size_t offset) const assumptious -> THandlePair {
         THandlePair copy = *this;
         return (copy -= offset);
      }

      /// Offset first element to the left by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return reference to this, after being modified                   
      constexpr auto operator -= (size_t offset) assumptious -> THandlePair& {
         key -= offset;
         val -= offset;
         return *this;
      }

      /// Prefix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      constexpr auto operator -- () assumptious -> THandlePair& {
         return (*this -= 1);
      }

      /// Suffix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      constexpr auto operator -- (int) assumptious -> THandlePair {
         THandlePair backup = *this;
         *this -= 1;
         return backup;
      }
   };

   static_assert(not CT::Intent<THandlePair<Handle, Handle>>);*/

   template<CT::Handle K, CT::Handle V>
   THandlePair(K&&, V&&) -> THandlePair<Decay<K>, Decay<V>>;
}
