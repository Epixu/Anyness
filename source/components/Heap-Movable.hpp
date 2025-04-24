#pragma once
#include "../Container.hpp"
#include "../Allocator.hpp"
#include "../rtti/Intent.hpp"
#include <Langulus/TypeOf.hpp>
#include <Langulus/CT/Pooled.hpp>
#include <Langulus/CT/Allocatable.hpp>
#include <Langulus/CT/Referenced.hpp>
#include <Langulus/CT/Resolvable.hpp>
#include "DeepOwnership.hpp"


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
      union {
         char*  mReadableHeap;
         Byte*  mHeap = nullptr;
         void** mSparseHeap;
      };
      
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
                  self.mHeap = self.GetAllocation()->GetBlockStart();

                  if (previous.GetCount()) {
                     // Memory moved, and we should move all elements   
                     // in it. We're moving to new memory, so no reverse
                     // is required                                     
                     self.CreateWithIntent(Abandon(previous));
                     previous.Free();
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

      /// Emplace a new item at the first element, with or without an intent  
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      ///   @param rhs_with_intent - constructor argument. If this container  
      ///      is statically typed, this can be any constructor argument,     
      ///      otherwise it has to be an instance of the container type       
      template<CT::Container C>
      void EmplaceWithIntent(this C& self, auto&& rhs_with_intent) {
         using S  = IntentOf<decltype(rhs_with_intent)>;
         using ST = TypeOf<S>;
         AssumeDev(self.IsTyped(), HERE(), "Invalid type");
         AssumeDev(self.mHeap,     HERE(), "Invalid heap");
         auto& rhs = DeintCast(rhs_with_intent);

         if constexpr (C::TypeErased) {
            //                                                          
            // This container is type-erased                            
            //                                                          
            if (self.mType.IsSparse()) {
               AssumeDev(CT::Sparse<ST>, "Sparseness mismatch");
               using DT = Deptr<ST>;

               if constexpr (S::Shallow) {
                  // Do a refer/copy/disown/abandon/move sparse LHS     
                  if constexpr (CT::Null<ST>) {
                     // RHS is nullptr                                  
                     *self.mSparseHeap = nullptr;

                     if constexpr (DeeplyOwned<C>)
                        *self.GetEntry() = nullptr;
                  }
                  else {
                     // RHS is (maybe) valid pointer                    
                     AssumeDev(CT::Void<DT> or self.template IsSimilar<ST>(), HERE(),
                        "Type mismatch");

                     *self.mSparseHeap = rhs;

                     if constexpr (DeeplyOwned<C>)
                        self.template DeepKeep<S>();
                  }
               }
               else {
                  //TODO clone pointers
                  TODO();
               }
            }
            else {
               // Do a refer/copy/disown/abandon/move/clone dense LHS   
               AssumeDev(CT::Dense<ST>, "Sparseness mismatch");

               if constexpr (CT::Moved<S>)
                  self.mType.MoveConstruct   (self.mHeap, &rhs);
               else if constexpr (CT::Abandoned<S>)
                  self.mType.AbandonConstruct(self.mHeap, &rhs);
               else if constexpr (CT::Referred<S>)
                  self.mType.ReferConstruct  (self.mHeap, &rhs);
               else if constexpr (CT::Copied<S>)
                  self.mType.CopyConstruct   (self.mHeap, &rhs);
               else if constexpr (CT::Disowned<S>)
                  self.mType.DisownConstruct (self.mHeap, &rhs);
               else if constexpr (CT::Cloned<S>)
                  self.mType.CloneConstruct  (self.mHeap, &rhs);
               else
                  static_assert(false, "Unsupported intent");
            }
         }
         else {
            //                                                          
            // This container is statically-typed                       
            //                                                          
            using T = TypeOf<C>;

            if constexpr (S::Shallow and CT::Sparse<T>) {
               // Do a copy/refer/disown/abandon/move sparse RHS        
               if constexpr (CT::Null<ST>) {
                  // RHS is nullptr                                     
                  *self.mSparseHeap = nullptr;

                  if constexpr (DeeplyOwned<C>)
                     *self.GetEntry() = nullptr;
               }
               else if constexpr (CT::ConstructibleFrom<T, ST>) {
                  *self.mSparseHeap = rhs;

                  if constexpr (DeeplyOwned<C>)
                     self.template DeepKeep<S>();
               }
               else static_assert(false, "Can't construct sparse T");
            }
            else if constexpr (CT::Dense<T>) {
               // Do a copy/disown/abandon/move/clone inside a dense    
               // handle                                                
               if constexpr (CT::ConstructibleFrom<T, S>)
                  new (self.mHeap) Decay<T> (S::Nest(rhs));
               else
                  static_assert(false, "Can't construct dense T");
            }
            else if constexpr (CT::Dense<Deptr<T>>) {
               // Clone sparse data with exactly one pointer            
               if constexpr (CT::Resolvable<Decay<T>>) {
                  // If T is resolvable, we need to always clone the    
                  // resolved (a.k.a the most concrete) type            
                  TODO();
               }
               else {
                  // Otherwise attempt cloning DT conventionally        
                  static_assert(CT::Similar<T, ST>, "Type mismatch");
                  auto meta = MetaDataOf<Decay<T>>();
                  auto entry = Allocator::Allocate(meta, meta.RequestSize(1).mByteSize);
                  auto pointer = entry->GetBlockStart();
                  try {
                     IntentNew(pointer, S::Nest(*DeintCast(rhs)));
                  }
                  catch (...) {
                     Allocator::Deallocate(entry);
                     return;
                  }

                  *self.mSparseHeap = pointer;

                  if constexpr (DeeplyOwned<C>)
                     *self.GetEntry() = entry;
               }
            }
            else {
               // Clone sparse data with more than one pointer          
               // Clone indirection layers by nesting                   
               TODO();
            }
         }
      }
      
      /// Reassign new value to the first element, with or without an intent  
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      ///   @param rhs_with_intent - container to assign from?                
      template<CT::Container C>
      void AssignWithIntent(this C& self, CT::Container auto&& rhs_with_intent) {
         using S  = IntentOf<decltype(rhs_with_intent)>;
         using ST = TypeOf<S>;
         using STT = TypeOf<ST>;
         AssumeDev(self.IsTyped(), HERE(), "Invalid type");
         AssumeDev(self.mHeap,     HERE(), "Invalid heap");
         auto& rhs = DeintCast(rhs_with_intent);

         if constexpr (C::TypeErased) {
            //                                                          
            // This container is type-erased                            
            //                                                          
            if (self.mType.IsSparse()) {
               AssumeDev(rhs.IsSparse(), "Sparseness mismatch");

               if constexpr (S::Shallow) {
                  // Do a refer/copy/disown/abandon/move sparse LHS     
                  *self.mSparseHeap = *rhs.mSparseHeap;
                  if constexpr (DeeplyOwned<C, ST>)
                     *self.GetEntry() = *rhs.GetEntry();

                  if constexpr (S::ResetsOnMove) {
                     *rhs.mSparseHeap = nullptr;
                     if constexpr (DeeplyOwned<C, ST>)
                        *rhs.GetEntry() = nullptr;
                  }

                  if constexpr (DeeplyOwned<C>) {
                     if constexpr (DeeplyOwned<ST>)
                        self.template DeepKeep<S>(*rhs.GetEntry());
                     else
                        self.template DeepKeep<S>(nullptr);
                  }
               }
               else {
                  //TODO clone pointers
                  TODO();
               }
            }
            else {
               // Do a refer/copy/disown/abandon/move/clone dense LHS   
               AssumeDev(CT::Dense<STT>, "Sparseness mismatch");

               if constexpr (CT::Moved<S>)
                  self.mType.MoveAssign   (self.mHeap, rhs.mHeap);
               else if constexpr (CT::Abandoned<S>)
                  self.mType.AbandonAssign(self.mHeap, rhs.mHeap);
               else if constexpr (CT::Referred<S>)
                  self.mType.ReferAssign  (self.mHeap, rhs.mHeap);
               else if constexpr (CT::Copied<S>)
                  self.mType.CopyAssign   (self.mHeap, rhs.mHeap);
               else if constexpr (CT::Disowned<S>)
                  self.mType.DisownAssign (self.mHeap, rhs.mHeap);
               else if constexpr (CT::Cloned<S>)
                  self.mType.CloneAssign  (self.mHeap, rhs.mHeap);
               else
                  static_assert(false, "Unsupported intent");
            }
         }
         else {
            //                                                          
            // This container is statically-typed                       
            //                                                          
            using T = TypeOf<C>;

            if constexpr (S::Shallow and CT::Sparse<T>) {
               // Do a copy/refer/disown/abandon/move sparse RHS        
               if constexpr (CT::AssignableFrom<T, STT>) {
                  *self.mSparseHeap = *rhs.mSparseHeap;
                  if constexpr (DeeplyOwned<C, ST>)
                     *self.GetEntry() = *rhs.GetEntry();

                  if constexpr (S::ResetsOnMove) {
                     *rhs.mSparseHeap = nullptr;
                     if constexpr (DeeplyOwned<C, ST>)
                        *rhs.GetEntry() = nullptr;
                  }

                  if constexpr (DeeplyOwned<C>) {
                     if constexpr (DeeplyOwned<ST>)
                        self.template DeepKeep<S>(*rhs.GetEntry());
                     else
                        self.template DeepKeep<S>(nullptr);
                  }
               }
               else static_assert(false, "Can't construct sparse T");
            }
            else if constexpr (CT::Dense<T>) {
               // Do a copy/disown/abandon/move/clone inside a dense    
               // handle                                                
               if constexpr (CT::AssignableFrom<T, typename S::template As<STT>>)
                  *self.GetRaw() = S::Nest(*rhs.GetRaw());
               else
                  static_assert(false, "Can't construct dense T");
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
                  IntentAssign(pointer, S::Nest(*rhs->Get()));

                  *self.GetRaw() = pointer;
                  if constexpr (DeeplyOwned<C>)
                     *rhs.GetEntry() = entry;
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
      auto GetItem(this C&&) has_assumptions -> Deep<C>;

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
