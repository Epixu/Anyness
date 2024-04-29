///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "Handle.hpp"

#define TEMPLATE() template<class T, bool EMBED>
#define HAND() Handle<T, EMBED>


namespace Langulus::Anyness
{

   /// Create an embedded handle while searching for memory entry             
   ///   @param v - pointer to the element's position inside a Block          
   TEMPLATE() LANGULUS(INLINED)
   constexpr HAND()::Handle(ValueType v) noexcept requires (Embedded or Sparse)
      : mValue {v} {
      if constexpr (TypeErased or CT::Allocatable<T>)
         mEntry = Allocator::Find(MetaDataOf<T>(), mValue);
      else
         mEntry = nullptr;
   }

   /// Create an embedded handle by manually specifying entry                 
   ///   @param v - pointer to the element's position inside a Block          
   ///   @param e - pointer to the element's entry inside a Block             
   TEMPLATE() LANGULUS(INLINED)
   constexpr HAND()::Handle(ValueType v, EntryType e) noexcept requires (Embedded or Sparse)
      : mValue {v}
      , mEntry {e} {}

   /// Create an embedded handle                                              
   ///   @param v - a reference to the element                                
   ///   @param e - a reference to the element's entry                        
   /*TEMPLATE() LANGULUS(INLINED)
   constexpr HAND()::Handle(T& v, AllocType& e) IF_UNSAFE(noexcept)
   requires (EMBED and CT::Sparse<T> and CT::Mutable<T>)
      : mValue {&v}
      , mEntry {&e} {
      static_assert(CT::NotHandle<T>, "Handles can't be nested");
   }
      
   TEMPLATE() LANGULUS(INLINED)
   constexpr HAND()::Handle(const T& v, const AllocType& e) IF_UNSAFE(noexcept)
   requires (EMBED and CT::Sparse<T> and not CT::Mutable<T>)
      : mValue {&v}
      , mEntry {&e} {
      static_assert(CT::NotHandle<T>, "Handles can't be nested");
   }
      
   /// Create an embedded handle                                              
   ///   @param v - a reference to the element                                
   ///   @param e - the entry (optional)                                      
   TEMPLATE() LANGULUS(INLINED)
   constexpr HAND()::Handle(T& v, AllocType e) IF_UNSAFE(noexcept)
   requires (EMBED and CT::Dense<T> and CT::Mutable<T>)
      : mValue {&v}
      , mEntry { e} {
      static_assert(CT::NotHandle<T>, "Handles can't be nested");
   }

   TEMPLATE() LANGULUS(INLINED)
   constexpr HAND()::Handle(const T& v, AllocType e) IF_UNSAFE(noexcept)
   requires (EMBED and CT::Dense<T> and not CT::Mutable<T>)
      : mValue {&v}
      , mEntry { e} {
      static_assert(CT::NotHandle<T>, "Handles can't be nested");
   }

   /// Create an embedded handle (automatically find allocation)              
   ///   @param v - a reference to the element                                
   TEMPLATE() LANGULUS(INLINED)
   constexpr HAND()::Handle(T& v) IF_UNSAFE(noexcept)
   requires (EMBED and CT::Dense<T> and CT::Mutable<T>)
      : mValue {&v} {
      static_assert(CT::NotHandle<T>, "Handles can't be nested");
      if constexpr (CT::Allocatable<T>)
         mEntry = Allocator::Find(MetaDataOf<T>(), &mValue);
      else
         mEntry = nullptr;
   }

   TEMPLATE() LANGULUS(INLINED)
   constexpr HAND()::Handle(const T& v) IF_UNSAFE(noexcept)
   requires (EMBED and CT::Dense<T> and not CT::Mutable<T>)
      : mValue {&v} {
      static_assert(CT::NotHandle<T>, "Handles can't be nested");
      if constexpr (CT::Allocatable<T>)
         mEntry = Allocator::Find(MetaDataOf<T>(), &mValue);
      else
         mEntry = nullptr;
   }*/

   /// Semantically construct using handle of any compatible type             
   ///   @param other - the handle and semantic to construct with             
   TEMPLATE() template<template<class> class S, CT::Handle H>
   requires CT::SemanticMakable<S, T> LANGULUS(INLINED)
   constexpr HAND()::Handle(S<H>&& other)
      : mValue (S<T>(other->Get()))
      , mEntry {nullptr} {
      using HT = TypeOf<H>;
      static_assert(CT::Similar<T, HT>, "Type mismatch");

      if constexpr (not Embedded) {
         if constexpr (S<H>::Shallow) {
            // Copy/Refer/Disown/Move/Abandon a handle                  
            if constexpr (S<H>::Move) {
               // Always reset remote entry, when moving (if sparse)    
               if constexpr (CT::Sparse<HT>) {
                  mEntry = other->GetEntry();
                  other->GetEntry() = nullptr;
               }

               // Also reset remote value, if not an abandoned pointer  
               if constexpr (S<H>::Keep and CT::Sparse<HT>)
                  other->Get() = nullptr;
            }
         }
         else {
            // Clone a handle                                           
            TODO();
         }
      }
      else mEntry = other->GetEntry();
   }
      
   TEMPLATE() template<class A>
   constexpr HAND()::Handle(A&& argument) noexcept requires (not Embedded)
      : mValue (Forward<A>(argument))
      , mEntry {nullptr} {}

   /// Semantically construct using compatible non-handle type                
   ///   @param other - the handle and semantic to construct with             
   /*TEMPLATE() template<class T1>
   requires (not EMBED and CT::MakableFrom<T, T1>) LANGULUS(INLINED)
   constexpr HAND()::Handle(T1&& other, AllocType e)
      : mValue (Forward<T1>(other))
      , mEntry {e} {
      using S = SemanticOf<T1>;

      if constexpr (CT::Sparse<T>) {
         // A pointer on the stack can still contain an entry           
         if constexpr (S::Shallow) {
            // Copy/Refer/Disown/Move/Abandon a pointer                 
            // Since pointers don't have ownership, it's just a copy    
            // with an optional entry search, if not disowned, and if   
            // managed memory is enabled                                
            using DT = Deptr<T>;

            if constexpr (CT::Allocatable<DT> and (S::Keep or S::Move))
               mEntry = Allocator::Find(MetaDataOf<DT>(), mValue);
         }
         else {
            // Clone a pointer                                          
            TODO();
         }
      }
   }

   /// Semantically construct using compatible non-handle Ref type            
   ///   @param other - the handle and semantic to construct with             
   TEMPLATE() template<template<class> class S>
   requires (CT::Sparse<T> and CT::Semantic<S<Ref<Deptr<T>>>>)
   constexpr HAND()::Handle(S<Ref<Deptr<T>>>&& other)
      : Handle(S<Handle>::Nest(other->GetHandle())) {}
   
   /// Semantically construct using compatible non-handle Own type            
   ///   @param other - the handle and semantic to construct with             
   TEMPLATE() template<template<class> class S>
   requires (CT::Dense<T> and CT::Semantic<S<Own<T>>>)
   constexpr HAND()::Handle(S<Own<T>>&& other)
      : Handle(S<Handle>::Nest(other->GetHandle())) {}*/

   /// Handle destructor                                                      
   TEMPLATE()
   HAND()::~Handle() {
      if constexpr (not Embedded and not Sparse and CT::Referencable<T>) {
         // Will call value destructor at end of scope, just suppress   
         // the reference warning                                       
         IF_SAFE(mValue.Reference(-1));
      }
   }

   /// Compare a handle with a comparable value/handle                        
   ///   @attention this compares contents and isn't suitable for iteration   
   TEMPLATE()
   constexpr bool HAND()::operator == (const auto& rhs) const
   noexcept requires (not TypeErased or Sparse) {
      using RHS = Deref<decltype(rhs)>;
      if constexpr (CT::Handle<RHS> and CT::Comparable<T, TypeOf<RHS>>) {
         if constexpr (Embedded) {
            if constexpr (RHS::Embedded)  return *mValue == *rhs.mValue;
            else                          return *mValue == rhs.mValue;
         }
         else {
            if constexpr (RHS::Embedded)  return  mValue == *rhs.mValue;
            else                          return  mValue == rhs.mValue;
         }
      }
      else if constexpr (CT::Comparable<T, RHS>) {
         if constexpr (Embedded)
            return *mValue == rhs;
         else
            return  mValue == rhs;
      }
      else LANGULUS_ERROR("Can't compare");
   }

   /// Compare a handle with a comparable value                               
   ///   @attention this compares contents and isn't suitable for iteration   
   /*TEMPLATE() template<CT::NotHandle T1>
   requires CT::Comparable<T, T1> LANGULUS(INLINED)
   constexpr bool HAND()::operator == (const T1& rhs) const noexcept {
      if constexpr (Embedded)
         return *mValue == rhs;
      else
         return mValue == rhs;
   }
      
   /// Compare handles                                                        
   ///   @attention this compares contents and isn't suitable for iteration   
   TEMPLATE() template<class T1, bool EMBED1>
   requires CT::Comparable<T, T1> LANGULUS(INLINED)
   constexpr bool HAND()::operator == (const Handle<T1, EMBED1>& rhs) const noexcept {
      if constexpr (Embedded) {
         if constexpr (EMBED1)
            return *mValue == *rhs.mValue;
         else
            return *mValue == rhs.mValue;
      }
      else {
         if constexpr (EMBED1)
            return mValue == *rhs.mValue;
         else
            return mValue == rhs.mValue;
      }
   }*/
      
   /// Prefix increment operator                                              
   ///   @return the next handle                                              
   TEMPLATE() LANGULUS(INLINED)
   HAND()& HAND()::operator ++ () noexcept requires Embedded {
      ++mValue;
      if constexpr (CT::Sparse<T>)
         ++mEntry;
      return *this;
   }

   /// Prefix decrement operator                                              
   ///   @return the next handle                                              
   TEMPLATE() LANGULUS(INLINED)
   HAND()& HAND()::operator -- () noexcept requires Embedded {
      --mValue;
      if constexpr (CT::Sparse<T>)
         --mEntry;
      return *this;
   }

   /// Suffix increment operator                                              
   ///   @return the previous value of the handle                             
   TEMPLATE() LANGULUS(INLINED)
   HAND() HAND()::operator ++ (int) const noexcept requires Embedded {
      auto backup = *this;
      return ++backup;
   }

   /// Suffix decrement operator                                              
   ///   @return the previous value of the handle                             
   TEMPLATE() LANGULUS(INLINED)
   HAND() HAND()::operator -- (int) const noexcept requires Embedded {
      auto backup = *this;
      return --backup;
   }

   /// Offset the handle                                                      
   ///   @param offset - the offset to apply                                  
   ///   @return the offsetted handle                                         
   TEMPLATE() LANGULUS(INLINED)
   HAND() HAND()::operator + (Offset offset) const noexcept requires Embedded {
      auto backup = *this;
      return backup += offset;
   }

   /// Offset the handle                                                      
   ///   @param offset - the offset to apply                                  
   ///   @return the offsetted handle                                         
   TEMPLATE() LANGULUS(INLINED)
   HAND() HAND()::operator - (Offset offset) const noexcept requires Embedded {
      auto backup = *this;
      return backup -= offset;
   }
   
   /// Prefix increment operator                                              
   ///   @return the next handle                                              
   TEMPLATE() LANGULUS(INLINED)
   HAND()& HAND()::operator += (Offset offset) noexcept requires Embedded {
      mValue += offset;
      if constexpr (CT::Sparse<T>)
         mEntry += offset;
      return *this;
   }

   /// Prefix decrement operator                                              
   ///   @return the next handle                                              
   TEMPLATE() LANGULUS(INLINED)
   HAND()& HAND()::operator -= (Offset offset) noexcept requires Embedded {
      mValue -= offset;
      if constexpr (CT::Sparse<T>)
         mEntry -= offset;
      return *this;
   }

   /// Get a reference to the contents                                        
   TEMPLATE() LANGULUS(INLINED)
   typename HAND()::Type& HAND()::Get() noexcept {
      if constexpr (Embedded) return *mValue;
      else                    return  mValue;
   }
   
   TEMPLATE() LANGULUS(INLINED)
   typename HAND()::Type const& HAND()::Get() const noexcept {
      if constexpr (Embedded) return *mValue;
      else                    return  mValue;
   }
   
   /// Get the entry                                                          
   TEMPLATE() LANGULUS(INLINED)
   typename HAND()::AllocType& HAND()::GetEntry() noexcept {
      if constexpr (Embedded and Sparse)  return *mEntry;
      else                                return  mEntry;
   }

   TEMPLATE() LANGULUS(INLINED)
   typename HAND()::AllocType const& HAND()::GetEntry() const noexcept {
      if constexpr (Embedded and Sparse)  return *mEntry;
      else                                return  mEntry;
   }

   /// Assign a new pointer and entry at the handle                           
   ///   @attention this overwrites previous handle without dereferencing it, 
   ///      and without destroying anything                                   
   ///   @param pointer - the new pointer to assign                           
   ///   @param entry - the allocation that the pointer is part of            
   /*TEMPLATE() LANGULUS(INLINED)
   void HAND()::Create(T pointer, AllocType entry) noexcept requires CT::Sparse<T> {
      Get() = pointer;
      GetEntry() = entry;
   }
   
   /// Move-assign a new value and entry at the handle                        
   ///   @attention this overwrites previous handle without dereferencing it, 
   ///      and without destroying anything                                   
   ///   @param value - the new value to assign                               
   ///   @param entry - the allocation that the value is part of              
   TEMPLATE() LANGULUS(INLINED)
   void HAND()::Create(T&& value, AllocType entry) noexcept requires CT::Dense<T> {
      SemanticNew(&Get(), Move(value));
      GetEntry() = entry;
   }

   /// Refer-assign a new value and entry at the handle                       
   ///   @attention this overwrites previous handle without dereferencing it, 
   ///      and without destroying anything                                   
   ///   @param value - the new value to assign                               
   ///   @param entry - the allocation that the value is part of              
   TEMPLATE() LANGULUS(INLINED)
   void HAND()::Create(const T& value, AllocType entry) noexcept requires CT::Dense<T> {
      SemanticNew(&Get(), Refer(value));
      GetEntry() = entry;
   }*/

   /// Semantically instantiate anything at the handle                        
   ///   @attention this overwrites previous handle without dereferencing it, 
   ///      and without destroying anything - that's your responsibility      
   ///   @param rhs - what are we instantiating                               
   ///   @param type - type of the contained data, used only if handle is     
   ///      type-erased                                                       
   TEMPLATE() LANGULUS(INLINED)
   void HAND()::CreateSemantic(auto&& rhs, DMeta type) {
      using S  = SemanticOf<decltype(rhs)>;
      using ST = TypeOf<S>;

      if constexpr (TypeErased) {
         LANGULUS_ASSUME(DevAssumes, type,
            "Invalid type provided for type-erased handle");

         if (type->mIsSparse) {
            if constexpr (S::Shallow) {
               // Do a copy/disown/abandon/move sparse LHS              
               if constexpr (CT::Handle<ST>) {
                  // RHS is a handle                                    
                  using HT = TypeOf<ST>;
                  static_assert(CT::Sparse<T> == CT::Sparse<HT>);
                  Get() = rhs->Get();

                  if constexpr (S::Keep or S::Move)
                     GetEntry() = rhs->GetEntry();
                  else
                     GetEntry() = nullptr;

                  if constexpr (S::Move) {
                     // We're moving RHS, so we need to clear it up     
                     if constexpr (S::Keep)
                        rhs->Get() = nullptr;

                     // Clearing entry is mandatory, because we're      
                     // transferring the ownership                      
                     rhs->GetEntry() = nullptr;

                     if constexpr (not ST::Embedded and Embedded) {
                        // Moving from non-embedded pointer handle to an
                        // embedded one always references - it's like   
                        // pushing a raw pointer                        
                        if (GetEntry()) {
                           const_cast<Allocation*>(GetEntry())->Keep();
                           if (type->mReference)
                              type->mReference(Get(), 1);
                        }
                     }
                  }
                  else if constexpr (S::Keep and Embedded) {
                     // Copying RHS, keep it only if not disowning it   
                     if (GetEntry()) {
                        const_cast<Allocation*>(GetEntry())->Keep();
                        if (type->mReference)
                           type->mReference(Get(), 1);
                     }
                  }
               }
               else if constexpr (CT::Nullptr<ST>) {
                  // RHS is a simple nullptr                            
                  Get() = nullptr;
                  GetEntry() = nullptr;
               }
               else {
                  // RHS is not a handle, but we'll wrap it in a handle,
                  // in order to find its entry (if managed memory is   
                  // enabled)                                           
                  static_assert(CT::Sparse<T> == CT::Sparse<ST>);
                  HandleLocal<T> rhsh {rhs.Forward()};
                  Get() = rhsh.Get();
                  GetEntry() = rhsh.GetEntry();

                  if constexpr (S::Keep and Embedded) {
                     // Raw pointers are always referenced, even when   
                     // moved (as long as it's a keeper semantic)       
                     if (GetEntry()) {
                        const_cast<Allocation*>(GetEntry())->Keep();
                        if (type->mReference)
                           type->mReference(Get(), 1);
                     }
                  }
               }
            }
            else {
               //TODO clone pointers
               TODO();
            }
         }
         else {
            // Do a copy/disown/abandon/move/clone inside a dense handle
            if constexpr (CT::Handle<ST>) {
               // RHS is a handle                                       
               using HT = TypeOf<ST>;
               static_assert(CT::Sparse<T> == CT::Sparse<HT>);
               TODO();
            }
            else {
               static_assert(CT::Sparse<T> == CT::Sparse<ST>);

               if constexpr (S::Move) {
                  if constexpr (S::Keep)
                     type->mMoveAssigner(&Get(), &*rhs);
                  else
                     type->mAbandonAssigner(&Get(), &*rhs);
               }
               else if constexpr (S::Shallow) {
                  if constexpr (S::Keep) {
                     if constexpr (CT::Referred<S>)
                        type->mReferAssigner(&Get(), const_cast<void*>(reinterpret_cast<const void*>(&*rhs)));
                     else
                        type->mCopyAssigner(&Get(), &*rhs);
                  }
                  else type->mDisownAssigner(&Get(), &*rhs);
               }
               else type->mCloneAssigner(&Get(), &*rhs);
            }
         }
      }
      else {
         if constexpr (S::Shallow and CT::Sparse<T>) {
            // Do a copy/refer/disown/abandon/move sparse RHS           
            if constexpr (CT::Handle<ST>) {
               // RHS is a handle                                       
               using HT = TypeOf<ST>;
               static_assert(CT::Similar<T, HT>, "Handle type mismatch");
               Get() = rhs->Get();

               if constexpr (S::Keep or S::Move)
                  GetEntry() = rhs->GetEntry();
               else
                  GetEntry() = nullptr;

               if constexpr (S::Move) {
                  // We're moving RHS, so we need to clear it up        
                  if constexpr (S::Keep)
                     rhs->Get() = nullptr;

                  // Clearing entry is mandatory, because we're         
                  // transferring the ownership                         
                  rhs->GetEntry() = nullptr;
               }
               else if constexpr (S::Keep and Embedded) {
                  // Copying RHS, but keep it only if not disowning it  
                  if (GetEntry()) {
                     const_cast<Allocation*>(GetEntry())->Keep();
                     if constexpr (CT::Referencable<Deptr<T>>)
                        Get()->Reference(1);
                  }
               }
            }
            else if constexpr (CT::Nullptr<ST>) {
               // RHS is a simple nullptr                               
               Get() = nullptr;
               GetEntry() = nullptr;
            }
            else if constexpr (CT::MakableFrom<T, ST>) {
               using DT = Deptr<T>;
               Get() = DesemCast(rhs);
               if constexpr (CT::Allocatable<DT> and (S::Keep or S::Move))
                  GetEntry() = Allocator::Find(MetaDataOf<DT>(), Get());
               else
                  GetEntry() = nullptr;

               if constexpr (S::Keep and Embedded) {
                  // Raw pointers are always referenced, even when moved
                  // (as long as it's a keeper semantic)                
                  if (GetEntry()) {
                     const_cast<Allocation*>(GetEntry())->Keep();
                     if constexpr (CT::Referencable<Deptr<T>>)
                        Get()->Reference(1);
                  }
               }
            }
            else LANGULUS_ERROR("Can't initialize sparse T");
         }
         else if constexpr (CT::Dense<T>) {
            // Do a copy/disown/abandon/move/clone inside a dense handle
            if constexpr (CT::Handle<ST> and CT::MakableFrom<T, TypeOf<ST>>)
               new ((void*) &Get()) T(S::Nest(rhs->Get()));
            else if constexpr (CT::MakableFrom<T, S>)
               new ((void*) &Get()) T(S::Nest(rhs));
            else
               LANGULUS_ERROR("Can't initialize dense T");
         }
         else if constexpr (CT::Dense<Deptr<T>>) {
            // Clone sparse/dense data                                  
            if constexpr (CT::Resolvable<Decay<T>>) {
               // If T is resolvable, we need to always clone the       
               // resolved (a.k.a the most concrete) type               
               TODO();
            }
            else {
               // Otherwise attempt cloning DT conventionally           
               using DT = Decay<T>;
               auto meta = MetaDataOf<DT>();
               auto entry = Allocator::Allocate(meta, meta->RequestSize(1).mByteSize);
               auto pointer = entry->template As<DT>();

               if constexpr (CT::Handle<ST>) {
                  static_assert(CT::Similar<T, TypeOf<ST>>, "Type mismatch");
                  SemanticNew(pointer, S::Nest(*rhs->Get()));
               }
               else {
                  static_assert(CT::Similar<T, ST>, "Type mismatch");
                  SemanticNew(pointer, S::Nest(**rhs));
               }

               Get() = pointer;
               GetEntry() = entry;
            }
         }
         else {
            // Pointers of pointers                                     
            // Clone indirection layers by nesting                      
            TODO();
         }
      }
   }
   
   /// Refer-assign a new value and entry at the handle                       
   ///   @attention this overwrites previous handle without dereferencing it, 
   ///      and without destroying anything                                   
   ///   @param value - the new value to assign                               
   ///   @param entry - the allocation that the value is part of              
   TEMPLATE() LANGULUS(INLINED)
   void HAND()::Assign(const Type& value, AllocType entry) noexcept requires (Embedded and Mutable) {
      Get() = value;
      GetEntry() = entry;
   }
   
   /// Dereference/destroy the current handle contents, and set new ones      
   ///   @param rhs - new contents to assign                                  
   ///   @param type - type of the contained data, used only if handle is     
   ///      type-erased                                                       
   TEMPLATE() LANGULUS(INLINED)
   void HAND()::AssignSemantic(auto&& rhs, DMeta type) requires Mutable {
      using S = SemanticOf<decltype(rhs)>;
      Destroy(type);
      CreateSemantic(S::Nest(rhs), type);
   }
   
   /// Swap any two handles, often this is embedded, while rhs is not         
   ///   @param rhs - right hand side                                         
   ///   @param type - type of the contained data, used only if handle is     
   ///      type-erased                                                       
   TEMPLATE() LANGULUS(INLINED)
   void HAND()::Swap(CT::Handle auto& rhs, DMeta type) requires Mutable {
      using RHS = Deref<decltype(rhs)>;

      if constexpr (Sparse) {
         std::swap(Get(), rhs.Get());
         std::swap(GetEntry(), rhs.GetEntry());

         if constexpr (not Embedded and RHS::Embedded) {
            if (rhs.GetEntry()) {
               const_cast<Allocation*>(rhs.GetEntry())->Keep();
               if constexpr (CT::Referencable<Deptr<T>>)
                  rhs.Get()->Reference(1);
            }
         }

         /*if (GetEntry() != rhs.GetEntry()) {
            if (GetEntry()) {
               const_cast<Allocation*>(GetEntry())->Keep();
               if constexpr (CT::Referencable<Deptr<T>>)
                  Get()->Reference(1);
            }

            if (rhs.GetEntry()) {
               const_cast<Allocation*>(rhs.GetEntry())->Keep();
               if constexpr (CT::Referencable<Deptr<T>>)
                  rhs.Get()->Reference(1);
            }
         }*/
      }
      else {
         HandleLocal<T> tmp {Abandon(*this)};
         Destroy<false, true>(type);
         CreateSemantic(Abandon(rhs), type);
         rhs.CreateSemantic(Abandon(tmp), type);
      }
   }

   /// Compare the contents of the handle with content                        
   ///   @param rhs - data to compare against                                 
   ///   @param type - type of the contained data, used only if handle is     
   ///      type-erased                                                       
   ///   @return true if contents are equal                                   
   TEMPLATE() LANGULUS(INLINED)
   bool HAND()::Compare(const auto& rhs, DMeta type) const {
      using RHS = Deref<decltype(rhs)>;

      if constexpr (CT::Handle<RHS>)
         return Get() == rhs.Get();
      else
         return Get() == rhs;
   }

   /// Reset the handle, by dereferencing entry, and destroying value, if     
   /// entry has been fully dereferenced                                      
   /// Does absolutely nothing for dense handles, they are destroyed when     
   /// handle is destroyed                                                    
   ///   @tparam RESET - whether or not to reset pointers to null             
   ///   @tparam DEALLOCATE - are we allowed to deallocate the memory?        
   ///   @param meta - type of the contained data, used only if handle is     
   ///      type-erased                                                       
   TEMPLATE() template<bool RESET, bool DEALLOCATE>
   void HAND()::Destroy(DMeta meta) const requires Mutable {
      if constexpr (TypeErased) {
         LANGULUS_ASSUME(DevAssumes, meta,
            "Invalid type provided for type-erased handle");

         if constexpr (CT::Sparse<T>) {
            // Handle is sparse, we should handle each indirection layer
            LANGULUS_ASSUME(DevAssumes, meta->mIsSparse,
               "Provided meta must match T sparseness");

            if (GetEntry()) {
               if (1 == GetEntry()->GetUses()) {
                  // This is the last occurence of that element         
                  LANGULUS_ASSUME(DevAssumes, Get(), "Null pointer");

                  if (meta->mDeptr->mIsSparse) {
                     // Pointer to pointer                              
                     // Release all nested indirection layers           
                     HandleLocal<Deptr<T>> {Get()}.Destroy(meta->mDeptr);
                  }
                  else if (meta->mDestructor) {
                     // Pointer to a complete, destroyable dense        
                     // Call the destructor                             
                     if (meta->mReference) {
                        if (meta->mReference(Get(), -1) == 0)
                           meta->mDestructor(Get());
                        else {
                           LANGULUS_OOPS(Destruct,
                              "Destroying a referenced element, "
                              "that's still in use");
                        }
                     }

                     meta->mDestructor(Get());
                  }

                  if constexpr (DEALLOCATE)
                     Allocator::Deallocate(const_cast<Allocation*>(GetEntry()));
               }
               else {
                  // This element occurs in more than one place         
                  // We're not allowed to deallocate the memory behind  
                  // it, but we must call destructors if T is           
                  // referencable, and its individual references have   
                  // reached 0. This usually happens when elements from 
                  // a THive are referenced.                            
                  if (not meta->mDeptr->mIsSparse and meta->mReference) {
                     if (meta->mReference(Get(), -1) == 0)
                        meta->mDestructor(Get());
                  }

                  const_cast<Allocation*>(GetEntry())->Free();
               }
            }

            if constexpr (RESET) {
               // Handle is dense and embedded, we should call remote   
               // destructor, but don't touch the entry, its irrelevant 
               Get() = nullptr;
               GetEntry() = nullptr;
            }
         }
      }
      else {
         using DT = Decay<T>;
         if constexpr (CT::Sparse<T>) {
            // Handle is sparse, we should handle each indirection layer
            if (GetEntry()) {
               if (1 == GetEntry()->GetUses()) {
                  // This is the last occurence of that element         
                  LANGULUS_ASSUME(DevAssumes, Get(), "Null pointer");

                  if constexpr (CT::Sparse<Deptr<T>>) {
                     // Pointer to pointer                              
                     // Release all nested indirection layers           
                     HandleLocal<Deptr<T>> {*Get()}.Destroy();
                  }
                  else if constexpr (CT::Destroyable<DT>) {
                     // Pointer to a complete, destroyable dense        
                     // Call the destructor                             
                     if constexpr (CT::Referencable<DT>) {
                        if (Get()->Reference(-1) == 0)
                           Get()->~DT();
                        else {
                           LANGULUS_OOPS(Destruct,
                              "Destroying a referenced element, "
                              "that's still in use");
                        }
                     }
                     else Get()->~DT();
                  }

                  if constexpr (DEALLOCATE)
                     Allocator::Deallocate(const_cast<Allocation*>(GetEntry()));
               }
               else {
                  // This element occurs in more than one place         
                  // We're not allowed to deallocate the memory behind  
                  // it, but we must call destructors if T is           
                  // referencable, and its individual references have   
                  // reached 1. This usually happens when elements from 
                  // a THive are referenced.                            
                  if constexpr (CT::Dense<Deptr<T>> and CT::Referencable<DT>) {
                     if (Get()->Reference(-1) == 0)
                        Get()->~DT();
                  }

                  const_cast<Allocation*>(GetEntry())->Free();
               }
            }

            if constexpr (RESET) {
               const_cast<Type&>(Get()) = nullptr;
               const_cast<AllocType&>(GetEntry()) = nullptr;
            }
         }
         else if constexpr (EMBED and CT::Destroyable<DT>) {
            // Handle is dense and embedded, we should call the remote  
            // destructor, but don't touch the entry, its irrelevant    
            if constexpr (CT::Referencable<DT>)
               Get().Reference(-1);
            Get().~DT();
         }
      }
   }

} // namespace Langulus::Anyness

#undef TEMPLATE
#undef HAND
