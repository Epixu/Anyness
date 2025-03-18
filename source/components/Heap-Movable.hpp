#pragma once
#include "../Container.hpp"
#include "../Allocator.hpp"
#include "../rtti/Intent.hpp"
#include <Langulus/TypeOf.hpp>
#include <Langulus/CT/Pooled.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Interfaces a heap allocation                                           
   /// Adds a pointer member to the raw byte memory                           
   /// The pointer is allowed to move on reallocation                         
   ///   @tparam ID - multiple heap interfaces are supported                  
   ///                                                                        
   template<unsigned ID = 0>
   struct HeapMovable {
   protected:
      template<unsigned, class>
      friend struct ReserveHeap;

      using Byte = ::std::uint8_t;
      template<CT::Container C>
      using View = typename C::ViewType;
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;
      template<CT::Container C>
      using Pick = Tif<CT::Mutable<C>, typename Deref<C>::PickMut, typename Deref<C>::Pick>;

      // The raw pointer                                                
      Byte* mHeap = nullptr;
      
      /// Get a size based on reflected allocation page and count             
      ///   @param count - the number of elements to request                  
      ///   @return both the provided byte size and reserved count            
      template<CT::Container C>
      auto RequestSize(this const C& self, const Count<C> count) has_assumptions -> Allocation::Request {
         using T = TypeOf<C>;
         Allocation::Request result;

         if constexpr (C::TypeErased) {
            AssumeDev(self.mType, HERE(),
               "Requesting allocation size for an untyped container");

            // Check for reflected minimal allocation at runtime        
            result.mByteSize = Roof2(::std::max(count * self.mType.GetSize(), self.mType.GetMinAlloc()));
            result.mElementCount = result.mByteSize / self.mType.GetSize();
         }
         else {
            // Check for reflected minimal allocation at compile-time   
            result.mByteSize = Roof2(::std::max(count * sizeof(T), CT::GetMinAlloc<T>()));
            result.mElementCount = result.mByteSize / sizeof(T);
         }

         return result;
      }

      /// Allocate a number of elements, relying on the type of the container 
      ///   @attention assumes a valid and non-abstract type, if dense        
      ///   @tparam CREATE - true to call constructors and set count          
      ///   @tparam SETSIZE - true to set count, despite not constructing     
      ///   @param elements - number of elements to allocate                  
      template<bool CREATE = false, bool SETSIZE = false, CT::Container C>
      void AllocateMore(this C& self, const Count<C> elements) {
         AssumeDev(elements > self.GetCount(), HERE(), "Bad element count");

         if constexpr (not C::TypeErased) {
            // Allocate/reallocate                                      
            using T = TypeOf<C>;
            const auto request = self.RequestSize(elements);
            if (self.GetAllocation()) {
               if (self.GetReserved() >= elements) {
                  // Required memory is already available               
                  if constexpr (CREATE) {
                     // But is not yet initialized, so initialize it    
                     if (self.GetCount() < elements) {
                        const auto count = elements - self.GetCount();
                        self.CropInner(self.GetCount(), count).CreateDefault();
                     }
                  }

                  if constexpr (CREATE or SETSIZE)
                     self.SetCount(elements);
                  return;
               }

               AssumeDev(self.GetUses() == 1, HERE(),
                  "Can't reuse memory of a heap used from multiple places, "
                  "BranchOut should've been called prior to AllocateMore"
               );

               // Reallocate                                            
               View<C> previous {self};
               auto reallocated = Allocator::Reallocate(
                  request.mByteSize * (C::Sparse ? 2 : 1),
                  self.GetAllocation()
               );
               Assert(reallocated, HERE(), "Out of memory");
               self.SetAllocation(reallocated);
               self.SetReserved(request.mElementCount);

               if (self.GetAllocation() != previous.GetAllocation()) {
                  if (not previous.GetCount()) {
                     // Memory moved, but nothing was initialized, so   
                     // just update heap pointer                        
                     self.mHeap = self.GetAllocation()->GetBlockStart();
                  }
                  else {
                     // Memory moved, and we should move all elements   
                     // in it. We're moving to new memory, so no reverse
                     // is required                                     
                     if constexpr (CT::AbandonConstructible<T>
                                or CT::MoveConstructible<T>
                                or CT::ReferConstructible<T>
                                or CT::CopyConstructible<T>
                     ) {
                        self.mHeap = self.GetAllocation()->GetBlockStart();
                        self.CreateWithIntent(Abandon(previous));
                        previous.Free();
                     }
                     else throw Exception {
                        "Memory moved, but T is not move-constructible", HERE()
                     };
                  }
               }
               else {
                  // Memory didn't move, but reserved count changed     
                  if constexpr (C::Sparse) {
                     // Move entry data to its new place                
                     MoveMemory(GetEntries(), previous.GetEntries(), self.GetCount());
                  }
               }

               if constexpr (CREATE) {
                  // Default-construct the rest                         
                  const auto count = elements - self.GetCount();
                  self.CropInner(self.GetCount(), count).CreateDefault();
               }
            }
            else {
               // Allocate a fresh set of elements                      
               self.template SetTypeInner<T>();
               self.AllocateFresh(request);

               if constexpr (CREATE) {
                  // Default-construct everything                       
                  self.CropInner(self.GetCount(), elements).CreateDefault();
               }
            }
         }
         else {
            Assert(self.mType, HERE(),
               "Can't instantiate unknown type");
            Assert(self.mType.IsSparse() or not self.mType.IsAbstract(), HERE(),
               "Unable to instantiate ", elements, " elements of abstract type ", self.mType);

            if (self.GetReserved() >= elements) {
               // Required memory is already available                  
               if constexpr (CREATE) {
                  // But is not yet initialized, so initialize it       
                  if (self.GetCount() < elements) {
                     const auto count = elements - self.GetCount();
                     self.CropInner(self.GetCount(), count).CreateDefault();
                  }
               }
            }
            else AllocateInner<CREATE>(elements);
         }

         if constexpr (CREATE or SETSIZE)
            self.SetCount(elements);
      }

      /// Shrink the block, depending on currently reserved	elements          
      /// Initialized elements on the back will be destroyed                  
      ///   @attention assumes 'elements' is smaller than the current reserve 
      ///   @param elements - number of elements to allocate                  
      template<CT::Container C>
      void AllocateLess(this C& self, const Count<C> elements) {
         AssumeDev(elements < self.GetReserved(), HERE(), "Bad element count");

         if (self.GetCount() > elements) {
            // Destroy back entries on smaller allocation               
            // Allowed even when container is static and out of         
            // jurisdiction, as in that case this acts as a simple      
            // count decrease, and no destructors shall be called       
            self.Trim(elements);
            return;
         }

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            // Shrink the memory block                                  
            // Guaranteed that entry doesn't move                       
            const auto request = RequestSize(elements);
            if (request.mElementCount == mReserved)
               return;
         
            AssumeDev(mEntry->GetUses() == 1, HERE(),
               "Can't reuse memory of a block used from multiple places, "
               "BranchOut should've been called prior to AllocateMore"
            );

            if constexpr (not TypeErased) {
               if constexpr (Sparse) {
                  // Move entry data to its new place                   
                  MoveMemory(
                     GetEntries() - mReserved + request.mElementCount,
                     GetEntries(), mCount
                  );
               }

               mEntry = Allocator::Reallocate(
                  request.mByteSize * (Sparse ? 2 : 1),
                  self.GetAllocation()
               );
            }
            else {
               AssumeDev(mType, HERE(), "Invalid type");

               if (mType->mIsSparse) {
                  // Move entry data to its new place                   
                  MoveMemory(
                     GetEntries() - mReserved + request.mElementCount,
                     GetEntries(), mCount
                  );
               }

               mEntry = Allocator::Reallocate(
                  request.mByteSize * (mType->mIsSparse ? 2 : 1),
                  self.GetAllocation()
               );
            }

            mReserved = request.mElementCount;
         #endif
      }

      /// Instantiate anything at the handle, with or without an intent       
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      ///   @param where - pointer to the place of instantiation              
      ///   @param rhs - what are we instantiating?                           
      template<CT::Container C>
      void EmplaceWithIntent(this C& self, Byte* where, auto&& rhs) {
         using S  = IntentOf<decltype(rhs)>;
         using ST = TypeOf<S>;

         if constexpr (C::TypeErased) {
            AssumeDev(self.IsTyped(), "Invalid type");

            if (self.mType.IsSparse()) {
               if constexpr (S::Shallow) {
                  // Do a copy/disown/abandon/move sparse LHS           
                  if constexpr (CT::Nullptr<ST>) {
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
                        // moved (as long as it's a keeper intent)         
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
         else {
            if constexpr (S::Shallow and CT::Sparse<T>) {
               // Do a copy/refer/disown/abandon/move sparse RHS        
               if constexpr (CT::Nullptr<ST>) {
                  // RHS is a simple nullptr                            
                  Get() = nullptr;
                  GetEntry() = nullptr;
               }
               else if constexpr (CT::MakableFrom<T, ST>) {
                  using DT = Deptr<T>;
                  Get() = DeintCast(rhs);
                  if constexpr (CT::Allocatable<DT> and (S::Keep or S::Move))
                     GetEntry() = Allocator::Find(MetaDataOf<DT>(), Get());
                  else
                     GetEntry() = nullptr;

                  if constexpr (S::Keep and Embedded) {
                     // Raw pointers are always referenced, even when   
                     // moved (as long as it's a keeper intent)         
                     if (GetEntry()) {
                        const_cast<Allocation*>(GetEntry())->Keep();
                        if constexpr (CT::Referencable<Deptr<T>>)
                           DecvqCast(Get())->Reference(1);
                     }
                  }
               }
               else static_assert(false, "Can't initialize sparse T");
            }
            else if constexpr (CT::Dense<T>) {
               // Do a copy/disown/abandon/move/clone inside a dense    
               // handle                                                
               if constexpr (CT::MakableFrom<T, S>)
                  new ((void*) &Get()) T(S::Nest(rhs));
               else
                  static_assert(false, "Can't initialize dense T");
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
                  static_assert(CT::Similar<T, ST>, "Type mismatch");
                  IntentNew(pointer, S::Nest(**rhs));

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
      
      /// Instantiate anything at the handle, with or without an intent       
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      ///   @param where - pointer to the place of instantiation              
      ///   @param rhs - what are we instantiating?                           
      template<CT::Container C>
      void TransferWithIntent(this C& self, Byte* where, CT::Container auto&& rhs) {
         using S  = IntentOf<decltype(rhs)>;
         using ST = TypeOf<S>;

         if constexpr (C::TypeErased) {
            AssumeDev(self.IsTyped(), "Invalid type");

            if (self.mType.IsSparse()) {
               if constexpr (S::Shallow) {
                  // Do a copy/disown/abandon/move sparse LHS           
                  // RHS is a handle                                    
                  using HT = TypeOf<ST>;
                  static_assert(CT::Sparse<T> == CT::Sparse<HT>);
                  Get() = rhs->Get();

                  if constexpr (S::Keep or S::Move)
                     GetEntry() = rhs->GetEntry();
                  else
                     GetEntry() = nullptr;

                  if constexpr (S::Move) {
                     // We're moving from an embedded RHS, so we need   
                     // to clear it up - we're transferring ownership   
                     if constexpr (S::Keep)
                        rhs->Get() = nullptr;
                     rhs->GetEntry() = nullptr;
                  }
                  else if constexpr (S::Keep and Embedded) {
                     // Copying RHS, but keep it only if not disowning  
                     if (GetEntry()) {
                        const_cast<Allocation*>(GetEntry())->Keep();
                        if (type->mReference)
                           type->mReference(Get(), 1);
                     }
                  }
               }
               else {
                  //TODO clone pointers
                  TODO();
               }
            }
            else {
               // Do a copy/disown/abandon/move/clone inside a dense    
               // handle                                                
               using HT = TypeOf<ST>;
               static_assert(CT::Sparse<T> == CT::Sparse<HT>);
               TODO();
            }
         }
         else {
            if constexpr (S::Shallow and CT::Sparse<T>) {
               // Do a copy/refer/disown/abandon/move sparse RHS        
               using HT = TypeOf<ST>;
               static_assert(CT::Similar<T, HT>, "Handle type mismatch");
               Get() = rhs->Get();

               if constexpr (S::Keep or S::Move)
                  GetEntry() = rhs->GetEntry();
               else
                  GetEntry() = nullptr;

               if constexpr (S::Move) {
                  // We're moving from an embedded RHS, so we need      
                  // to clear it up - we're transferring ownership      
                  if constexpr (S::Keep)
                     rhs->Get() = nullptr;
                  rhs->GetEntry() = nullptr;
               }
               else if constexpr (S::Keep and Embedded) {
                  // Copying RHS, but keep it only if not disowning it  
                  if (GetEntry()) {
                     const_cast<Allocation*>(GetEntry())->Keep();
                     if constexpr (CT::Referencable<Deptr<T>>)
                        DecvqCast(Get())->Reference(1);
                  }
               }
            }
            else if constexpr (CT::Dense<T>) {
               // Do a copy/disown/abandon/move/clone inside a dense    
               // handle                                                
               if constexpr (CT::MakableFrom<T, TypeOf<ST>>)
                  new ((void*) &Get()) T(S::Nest(rhs->Get()));
               else
                  static_assert(false, "Can't initialize dense T");
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
                  static_assert(CT::Similar<T, TypeOf<ST>>, "Type mismatch");
                  IntentNew(pointer, S::Nest(*rhs->Get()));

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

   public:
      using CTTI_Component = Yes;

      /// Get a direct access to the heap memory                              
      ///   @returns the memory pointer                                       
      template<CT::Container C>
      auto GetRaw(this C&& self) noexcept {
         using T = TypeOf<C>;
         if constexpr (CT::Mutable<C>)
            return reinterpret_cast<      T*>(self.mHeap);
         else
            return reinterpret_cast<const T*>(self.mHeap);
      }

      /// Get a direct access to the heap memory as a different type          
      ///   @returns the memory pointer                                       
      template<class T, CT::Container C>
      auto GetRawAs(this C&& self) noexcept {
         if constexpr (CT::Mutable<C>)
            return reinterpret_cast<      T*>(self.mHeap);
         else
            return reinterpret_cast<const T*>(self.mHeap);
      }
      
      template<CT::Container C>
      auto Get(this C&&) has_assumptions -> Pick<C>;

      template<CT::NotVoid AS, CT::Container C>
      auto As(this C&& self) -> Pick<C>;

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      auto AsCast(this C const& self) -> AS;

      template<CT::Container C>
      auto GetItem(this C&&) has_assumptions->Deep<C>;

      template<CT::Container C>
      auto GetDeep(this C&&) noexcept -> Deep<C>*;

      template<CT::Container C>
      auto GetResolved(this C&&) -> Deep<C>;

      template<CT::Container C>
      auto GetDense(this C&&, Count<C> = CountMax<C>) -> Deep<C>;

      template<CT::Container C>
      auto operator * (this C&&) -> Deep<C>;
   };

} // namespace Langulus::Anyness::Component
