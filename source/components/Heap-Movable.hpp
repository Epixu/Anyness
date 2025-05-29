#pragma once
#include "Heap-Reference.hpp"
#include "../Allocator.hpp"
#include "../rtti/Intent.hpp"
#include <Langulus/CT/Index.hpp>
#include <Langulus/CT/Pooled.hpp>
#include <Langulus/CT/Resolvable.hpp>
#include "Iteration-Range.hpp"

/*#include "../Container.hpp"
#include <Langulus/TypeOf.hpp>
#include <Langulus/CT/Allocatable.hpp>
#include <Langulus/CT/Referenced.hpp>
#include <algorithm>*/


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Interfaces a heap allocation                                           
   /// Adds a pointer member to the raw byte memory                           
   /// The pointer is allowed to move on reallocation                         
   ///   @tparam ID - multiple heap interfaces are supported                  
   ///                                                                        
   template<unsigned ID = 0>
   struct HeapMovable : HeapReference<ID> {
      static constexpr bool HeapCanBeNull = true;

   protected:
      template<unsigned, class>
      friend struct ReserveHeap;
      template<unsigned>
      struct IterationOperators;

      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Pick = Tif<CT::Mutable<C>, typename Deref<C>::PickMut, typename Deref<C>::Pick>;
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;

      /*using Byte = ::std::uint8_t;*/
      template<CT::Container C>
      using View = typename C::ViewType;
            

      /// Get a size based on reflected allocation page and count             
      ///   @param count - the number of elements to request                  
      ///   @return both the provided byte size and reserved count            
      template<CT::Container C>
      auto RequestSize(this const C& self, const Count<C> count) has_assumptions
      -> Allocation::Request {
         using T = TypeOf<C>;
         Allocation::Request result;

         if constexpr (C::TypeErased) {
            AssumeDev(self.mType, HERE(),
               "Requesting allocation size for an untyped container");

            // Check for reflected minimal allocation at runtime        
            result.mByteSize = Roof2(::std::max<Count<C>>(
               count * self.mType.GetSize(), self.mType.GetMinAlloc()));
            result.mElementCount = result.mByteSize / self.mType.GetSize();
         }
         else {
            // Check for reflected minimal allocation at compile-time   
            result.mByteSize = Roof2(::std::max<Count<C>>(
               count * sizeof(T), CT::GetMinAlloc<T>()));
            result.mElementCount = result.mByteSize / sizeof(T);
         }

         return result;
      }
      
      /// Allocate a fresh allocation                                         
      ///   @attention changes allocation, heap pointer and reserve count only
      ///   @param request - request to fulfill                               
      template<CT::Container C>
      void AllocateFresh(this C& self, const Allocation::Request& request) {
         Allocation* al;
         if constexpr (C::TypeErased) {
            if constexpr (CT::DeeplyOwned<C>) {
               // Deeply owned sparse containers have additional memory 
               // allocated for each pointer's entry                    
               al = Allocator::Allocate(self.mType,
                  request.mByteSize * (self.mType.IsSparse() ? 2 : 1));
            }
            else {
               al = Allocator::Allocate(self.mType,
                  request.mByteSize);
            }
         }
         else {
            // Deeply owned sparse containers have additional memory    
            // allocated for each pointer's entry                       
            al = Allocator::Allocate(self.GetType(),
               request.mByteSize * (CT::DeeplyOwned<C> and C::Sparse ? 2 : 1));
         }

         Assert(al, HERE(), "Out of memory");
         self.SetAllocation(al);
         self.SetReserved(request.mElementCount);
      }

      /// Allocate a number of elements, relying on the type of the container 
      ///   @attention assumes a valid and non-abstract type, if dense        
      ///   @tparam CREATE - true to call constructors and set count          
      ///   @tparam SETSIZE - true to set count, despite not constructing     
      ///   @param elements - number of elements to allocate                  
      template<bool CREATE = false, bool SETSIZE = false, CT::Container C>
      void AllocateMore(this C& self, const Count<C> elements) {
         AssumeDev(elements > self.GetCount(), HERE(), "Bad element count");

         if constexpr (CT::Typed<C>) {
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
                  request.mByteSize * (CT::DeeplyOwned<C> and C::Sparse ? 2 : 1),
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
                     auto from = IterateHandles(previous).begin();
                     for (auto to : IterateHandles(self))
                        to.EmplaceWithIntent(Abandon(*(from++)));
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
               if constexpr (requires { self.mType; })
                  self.mType = MetaDataOf<T>();

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
                  if constexpr (CT::DeeplyOwned<C, ST>)
                     *self.GetEntry() = *rhs.GetEntry();

                  if constexpr (S::ResetsOnMove) {
                     *rhs.mSparseHeap = nullptr;
                     if constexpr (CT::DeeplyOwned<C, ST>)
                        *rhs.GetEntry() = nullptr;
                  }

                  if constexpr (CT::DeeplyOwned<C>) {
                     if constexpr (CT::DeeplyOwned<ST>)
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
                  if constexpr (CT::DeeplyOwned<C, ST>)
                     *self.GetEntry() = *rhs.GetEntry();

                  if constexpr (S::ResetsOnMove) {
                     *rhs.mSparseHeap = nullptr;
                     if constexpr (CT::DeeplyOwned<C, ST>)
                        *rhs.GetEntry() = nullptr;
                  }

                  if constexpr (CT::DeeplyOwned<C>) {
                     if constexpr (CT::DeeplyOwned<ST>)
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
                  if constexpr (CT::DeeplyOwned<C>)
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
      using HeapReference<ID>::HeapReference;
      using HeapReference<ID>::operator =;

      constexpr HeapMovable() noexcept
         : HeapReference<ID> {nullptr} {}

      /// Return a handle to the first element                                
      ///   @attention assumes T is of proper sparseness if not void          
      ///   @tparam T - the type of data we're accessing                      
      ///      use void to use the type of the container, if statically typed 
      template<class T = void, CT::Container C>
      auto GetHandle(this C&& self) has_assumptions {
         static_assert(not CT::Handle<T>, "T can't be a handle");
         static_assert(not CT::Reference<T>, "Strip references");
         using DC = Deref<C>;
         using TT = Tif<CT::Void<T>, TypeOf<DC>, T>;

         if constexpr (CT::Void<TT>) {
            // Type-erased handle                                       
            if constexpr (CT::DeeplyOwned<DC>) {
               // C is deeply owned, so each sparse element is coupled  
               // with an entry that points to its allocation. Dense    
               // elements simply refer to the container's allocation   
               return Handle {self.mHeap, self.GetEntries(), self.GetType()};
            }
            else {
               // C isn't deeply owned, so handles are just pointers    
               // They still need to be handles, so that they have the  
               // necessary insertion/emplacement interfaces            
               return HandleDisowned {self.mHeap, self.GetType()};
            }
         }
         else {
            // Typed handle required                                    
            static_assert(CT::NotVoid<TT>, "Logic error");
            static_assert(CT::Untyped<DC> or CT::Sparse<TypeOf<C>> == CT::Sparse<TT>,
               "Sparseness mismatch");

            if constexpr (CT::Untyped<DC>)
               AssumeDev(self.IsSparse() == CT::Sparse<TT>, HERE(), "Sparseness mismatch");

            if constexpr (CT::DeeplyOwned<DC>) {
               // C is deeply owned, so each sparse element is coupled  
               // with an entry that points to its allocation. Dense    
               // elements simply refer to the container's allocation   
               return THandle<TT&> {&self.template Get<TT>(), self.GetEntries()};
            }
            else {
               // C isn't deeply owned, so handles are just pointers    
               // They still need to be handles, so that they have the  
               // necessary insertion/emplacement interfaces            
               return THandleDisowned<TT&> {&self.template Get<TT>()};
            }
         }
      }

      template<CT::IndexedLinearly C>
      auto GetHandleAt(this C&& self, CT::Index auto at) has_assumptions {
         return self.GetHandle() + at;
      }

      template<CT::NotVoid AS, CT::Container C>
      auto As(this C&& self) -> Pick<C>;

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      auto AsCast(this C const& self) -> AS;

      template<CT::Container C>
      auto GetItem(this C&&) has_assumptions -> Deep<C>;

      /// A safe way to get the first deep entry                              
      /// Will utilize any statically typed deep containers, if available     
      ///   @attention ignores sparseness                                     
      ///   @return a pointer to the first deep item, or nullptr if not deep  
      template<CT::Container C>
      auto GetDeep(this C&& self) noexcept -> Deep<C>* {
         if (not self.IsDeep())
            return nullptr;
         return self.template Get<Deep<C>*>();
      }

      template<CT::Container C>
      auto GetResolved(this C&&) -> Deep<C>;

      template<CT::Container C>
      auto GetDense(this C&&, Count<C> = CountMax<C>) -> Deep<C>;
   };

} // namespace Langulus::Anyness::Component
