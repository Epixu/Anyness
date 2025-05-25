#pragma once
#include "../Container.hpp"
#include "Indexed-Linear.hpp"
//#include "DeepOwnership.hpp"
#include <Langulus/CT/Allocatable.hpp>
#include <Langulus/CT/Referenced.hpp>
#include <Langulus/CT/Resolvable.hpp>


namespace Langulus::Anyness::Component
{
   
   /// Check if container's elements are emplaceable using the provided       
   /// argument list. Use empty list to test if default-constructible         
   ///   @attention type-erased elements are always emplaceable, because      
   ///      all arguments will be encapsulated in a descriptor, and will fail 
   ///      at runtime if not reflected as descriptor-constructible           
   template<class C, class...A>
   concept RangeEmplaceable = CT::Container<C> and (
      C::TypeErased or ::std::constructible_from<TypeOf<C>, A...>
   );


   ///                                                                        
   /// Implements emplacement for containers                                  
   ///   @tparam ID - heap we're inserting to                                 
   template<unsigned ID = 0>
   struct Emplacement {
      using CTTI_Component = Yes;

      template<CT::Container C>
      static consteval bool Validate() {
         static_assert(C::VariableCount, "You can't emplace stuff in a "
            "container that doesn't provide variable count");
      }

   private:
      template<CT::Container C>
      using PickMut = typename Deref<C>::PickMut;
      
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

                     if constexpr (CT::DeeplyOwned<C>)
                        *self.GetEntry() = nullptr;
                  }
                  else {
                     // RHS is (maybe) valid pointer                    
                     AssumeDev(CT::Void<DT> or self.template IsSimilar<ST>(), HERE(),
                        "Type mismatch");

                     *self.mSparseHeap = rhs;

                     if constexpr (CT::DeeplyOwned<C>)
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

               //TODO calling these shouldn't have checks inside, only assumptions are allowed
               //because this function is often used in loops, and checking if these
               //constructors are available can be done once before the loop begins
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

                  if constexpr (CT::DeeplyOwned<C>)
                     *self.GetEntry() = nullptr;
               }
               else if constexpr (CT::ConstructibleFrom<T, ST>) {
                  *self.mSparseHeap = rhs;

                  if constexpr (CT::DeeplyOwned<C>)
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
         -> PickMut<C> requires RangeEmplaceable<C, A...>;

      /// Generic emplacement                                                 
      template<CT::Container C, class...A>
      auto Emplace(this C&, A&&...)
         -> PickMut<C> requires RangeEmplaceable<C, A...>;
   };

} // namespace Langulus::Anyness::Component
