#pragma once
#include "../Container.hpp"
#include "Indexed-Linear.hpp"
//#include "DeepOwnership.hpp"
#include <Langulus/CT/Allocatable.hpp>
#include <Langulus/CT/Referenced.hpp>
#include <Langulus/CT/Resolvable.hpp>


namespace Langulus::CT
{

   /// Check if container's elements are emplaceable using the provided       
   /// argument list. Use empty list to test if default-constructible         
   ///   @attention type-erased elements are always emplaceable, because      
   ///      all arguments will be encapsulated in a descriptor, and will fail 
   ///      at runtime if not reflected as descriptor-constructible           
   template<class C, class...A>
   concept RangeEmplaceable = Container<C> and (
      Untyped<C> or ::std::constructible_from<TypeOf<C>, A...>
   );

} // namespace Langulus::CT

namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Implements emplacement for containers                                  
   ///   @tparam ID - heap we're inserting to                                 
   ///                                                                        
   template<unsigned ID = 0>
   struct Emplacement {
      using CTTI_Component = Yes;

      constexpr Emplacement() noexcept = default;
      ignore_all_intents(Emplacement);

   protected:
      template<unsigned>
      friend struct HeapMovable;

      template<CT::Container C>
      using PickMut = typename Deref<C>::PickMut;
      
      /// Emplace a new item at the first element, with or without an intent  
      ///   @attention this overwrites previous handle without dereferencing  
      ///      it, and without destroying anything                            
      ///   @param rhs_with_intent - constructor argument. If this container  
      ///      is statically typed, this can be any constructor argument,     
      ///      otherwise it has to be an instance of the container type       
      template<CT::Container C, CT::Intent I>
      void EmplaceWithIntent(this C& self, I&& rhs_with_intent) {
         using IT = TypeOf<I>;
         AssumeDev(self.IsTyped(), HERE(), "Invalid type");
         AssumeDev(self.GetRaw(),  HERE(), "Invalid heap");
         decltype(auto) rhs = *rhs_with_intent;

         if constexpr (CT::Untyped<C>) {
            //                                                          
            // This container is type-erased                            
            //                                                          
            if (self.IsSparse()) {
               AssumeDev(CT::Sparse<IT>, "Sparseness mismatch");
               using DT = Deptr<IT>;

               if constexpr (I::IsShallow()) {
                  // Do a refer/copy/disown/abandon/move sparse LHS     
                  if constexpr (CT::Null<IT>) {
                     // RHS is nullptr                                  
                     *self.mSparseHeap = nullptr;

                     if constexpr (CT::DeeplyOwned<C>)
                        *self.GetEntry() = nullptr;
                  }
                  else {
                     // RHS is (maybe) valid pointer                    
                     AssumeDev(CT::Void<DT> or self.template IsSimilar<IT>(), HERE(),
                        "Type mismatch");

                     *self.mSparseHeap = rhs;

                     if constexpr (CT::DeeplyOwned<C>)
                        self.template DeepKeep<I>();
                  }
               }
               else {
                  //TODO clone pointers
                  TODO();
               }
            }
            else {
               // Do a refer/copy/disown/abandon/move/clone dense LHS   
               AssumeDev(CT::Dense<IT>, "Sparseness mismatch");
               auto T = self.GetType();

               //TODO calling these shouldn't have checks inside, only assumptions are allowed
               //because this function is often used in loops, and checking if these
               //constructors are available can be done once before the loop begins
               if constexpr (CT::Moved<I>)
                  T.RunMoveConstruct   (self.GetRaw(), &rhs);
               else if constexpr (CT::Abandoned<I>)
                  T.RunAbandonConstruct(self.GetRaw(), &rhs);
               else if constexpr (CT::Referred<I>)
                  T.RunReferConstruct  (self.GetRaw(), &rhs);
               else if constexpr (CT::Copied<I>)
                  T.RunCopyConstruct   (self.GetRaw(), &rhs);
               else if constexpr (CT::Disowned<I>)
                  T.RunDisownConstruct (self.GetRaw(), &rhs);
               else if constexpr (CT::Cloned<I>)
                  T.RunCloneConstruct  (self.GetRaw(), &rhs);
               else
                  static_assert(false, "Unrecognized intent");
            }
         }
         else {
            //                                                          
            // This container is statically-typed                       
            //                                                          
            using T = TypeOf<C>;

            if constexpr (I::IsShallow() and CT::Sparse<T>) {
               // Do a copy/refer/disown/abandon/move sparse RHS        
               if constexpr (CT::Null<IT>) {
                  // RHS is nullptr                                     
                  *self.mSparseHeap = nullptr;

                  if constexpr (CT::DeeplyOwned<C>)
                     *self.GetEntry() = nullptr;
               }
               else if constexpr (CT::ConstructibleFrom<T, IT>) {
                  *self.mSparseHeap = rhs;

                  if constexpr (CT::DeeplyOwned<C>)
                     self.template DeepKeep<I>();
               }
               else static_assert(false, "Can't emplace shallow pointer");
            }
            else if constexpr (CT::Dense<T>) {
               // Do a copy/disown/abandon/move/clone inside a dense    
               // handle                                                
               if constexpr (CT::ConstructibleFrom<T, I>)
                  new (self.GetRaw()) Decay<T> (FWD(rhs_with_intent));
               else
                  static_assert(false, "Can't emplace");
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
                  static_assert(CT::Similar<T, IT>, "Type mismatch");
                  auto meta = MetaDataOf<Decay<T>>();
                  auto entry = Allocator::Allocate(meta, meta.RequestSize(1).mByteSize);
                  auto pointer = entry->GetBlockStart();
                  try {
                     IntentNew(pointer, I::Nest(*rhs));
                  }
                  catch (...) {
                     Allocator::Deallocate(entry);
                     return;
                  }

                  *self.mSparseHeap = pointer;

                  if constexpr (CT::DeeplyOwned<C>)
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

   public:
      /// Emplacement at specific index                                       
      template<CT::IndexedLinearly C, class...A>
      auto EmplaceAt(this C&, CT::Index auto, A&&...)
         -> PickMut<C> requires CT::RangeEmplaceable<C, A...>;

      /// Generic emplacement                                                 
      template<CT::Container C, class...A>
      auto Emplace(this C&, A&&...)
         -> PickMut<C> requires CT::RangeEmplaceable<C, A...>;
   };

} // namespace Langulus::Anyness::Component
