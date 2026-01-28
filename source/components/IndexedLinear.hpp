///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>
#include <Langulus/CT/Signed.hpp>
#include <Langulus/CT/Contiguous.hpp>
#include <limits>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Provides random element access based on a linear index, that is        
   /// mapped directly onto contiguous memory                                 
   ///   @tparam ID the stack/heap we're indexing                             
   ///   @tparam INDEX_CONSTRAINT constrain the type of allowed indices.      
   ///      Leave as 'void' to allow for all the usual integer types          
   template<unsigned ID, class INDEX_CONSTRAINT>
   struct IndexedLinear {
      using CTTI_Component  = Yes<>;
      using CTTI_Contiguous = Yes<>;
      
      static constexpr bool Indexed = true;
      static constexpr int  ComponentPrecedence = 3000;

   protected:
      template<unsigned, class> friend struct Insertion;
      template<unsigned>        friend struct HeapMovable;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;
      template<CT::Container C>
      using Pick = Tmut<C, typename Deref<C>::PickMut, typename Deref<C>::Pick>;
      template<CT::Container C>
      using PickRange = Tmut<C, typename Deref<C>::PickRangeMut, typename Deref<C>::PickRange>;
      
      /// Convert an index to an offset.                                      
      /// Special indices will be contextualized.                             
      /// Unsigned/signed indices are directly forwarded without any overhead.
      ///   @param index the index to simplify                                
      ///   @return a simple element offset into contiguous memory            
      template<CT::Container C, CT::Index INDEX>
      constexpr auto SimplifyIndex(this C const& self, INDEX index)
      assumptious -> Count<C> {
         if constexpr      (::std::same_as<INDEX, Index::Inner::All>)
            static_assert(false, "Index::All can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Many>)
            static_assert(false, "Index::Many can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Single>)
            static_assert(false, "Index::Single can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::None>)
            static_assert(false, "Index::None can't be used here");
         else if constexpr (::std::same_as<INDEX, Index::Inner::Front>)
            return 0;
         else if constexpr (::std::same_as<INDEX, Index::Inner::Middle>)
            return self.GetCount() / 2;
         else if constexpr (::std::same_as<INDEX, Index::Inner::Back>)
            return self.GetCount();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Mode>)
            return self.GetIndexMode();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Biggest>)
            return self.GetIndexLargest();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Smallest>)
            return self.GetIndexSmallest();
         else if constexpr (::std::same_as<INDEX, Index::Inner::Random>)
            return self.GetIndexRandom();
         else if constexpr (::std::same_as<INDEX, Index::Inner::First>)
            return 0;
         else if constexpr (::std::same_as<INDEX, Index::Inner::Last>) {
            const auto count = self.GetCount();
            return count ? count - 1 : CountMax<C>;
         }
         else if constexpr (requires { index.index; }) {
            const auto c = self.GetCount();
            // If index is negative, wrap it around (if in range)       
            if (index.index < 0)
               return c + index.index >= 0 ? c + index.index : CountMax<C>;
            return index.index >= c ? CountMax<C> : index.index;

         }
         else if constexpr (CT::Integer<INDEX>) {
            // Unsafe, works only on assumptions                        
            // Using an integer index explicitly makes a statement,     
            // that you know what you're doing                          
            LglsAssumeUser(static_cast<Count<C>>(index) < self.GetCount(),
               "Integer index out of range");

            if constexpr (CT::Signed<INDEX>) {
               LglsAssumeUser(index >= 0,
                  "Integer index is below zero, "
                  "use Index::At for reverse indices instead"
               );
            }
            return index;
         }
         else static_assert(false, "Unsupported index type");
      }
      
      /// Select a contiguous region from the memory block. Unsafe and may    
      /// return memory that has not been initialized yet! The resulting      
      /// data will be disowned.                                              
      ///   @attention assumes container is typed and allocated               
      ///   @param start starting element index (included)                    
      ///   @param count number of sequential elements                        
      ///   @return the selected disowned contiguous range                    
      template<CT::Container C>
      auto SelectInner(this C&& self, Count<C> start, Count<C> count)
      assumptious -> Decay<C> {
         LglsAssumeDev(self.IsAllocated(), "Block is not allocated");
         LglsAssumeDev(self.IsTyped(),     "Block is not typed");
         LglsAssumeDev(count,              "Invalid count");
         
         Decay<C> result {Disown(self)};
         result.SetCountInner(count);
         result.SetHeapInner(result.template GetRawAs<uint8_t>() + start * result.GetStride());
         return result;
      }

      /// Same as above, but implies that count is the remainder              
      ///   @param start starting element index (included)                    
      ///   @return the selected disowned contiguous range                    
      template<CT::Container C>
      auto SelectInner(this C&& self, Count<C> start) assumptious -> Decay<C> {
         return self.SelectInner(start, self.GetCount() - start);
      }

   public:
      /// Subscript operator for accessing element at a specific index        
      ///   @param idx the index                                              
      ///   @return the picked element                                        
      template<CT::Container C>
      decltype(auto) operator[] (this C&& self, CT::Index auto&& idx) assumptious {
         return self.GetAt(idx);
      }

      /// Get reference to Nth element as sparse or dense, depending on T.    
      /// This is a lower-level routine that does only sparseness checking.   
      /// No conversion or copying occurs, only pointer arithmetic.           
      ///   @attention no type-safety                                         
      ///   @attention assumes the container is typed                         
      ///   @attention assumes the container is allocated                     
      ///   @tparam AS the type of data we're accessing - use void to use the 
      ///      type of the container, if statically typed                     
      ///   @param idx the index                                              
      ///   @return the picked element                                        
      template<class AS = void, CT::Container C>
      decltype(auto) GetAt(this C&& self, CT::Index auto&& idx) assumptious {
         static_assert(not CT::Handle<AS>,    "T can't be a handle");
         static_assert(not CT::Reference<AS>, "Strip references first");
         using TC  = LglsMutIf(C, TypeOf<C>);
         using TCP = LglsMutIf(C, TC*);
         using TH  = Tif<CT::Void<AS>, TC, AS>;
         using THP = LglsMutIf(C, TH*);

         // Get the first element without dereferencing anything        
         auto heap = self.GetHeapInner();

         // Offset it                                                   
         const auto offset = self.SimplifyIndex(idx);
         const auto byte_offset = self.GetStride() * offset;
         if constexpr (CT::Mutable<C>) {
            heap = reinterpret_cast<void*>(
               reinterpret_cast<uint8_t*>(heap) + byte_offset
            );
         }
         else {
            heap = reinterpret_cast<void const*>(
               reinterpret_cast<uint8_t const*>(heap) + byte_offset
            );
         }

         // Dereference it if we have to                                
         if constexpr (CT::Void<TH>)
            return heap;
         else if constexpr (CT::TypeErased<C>) {
            // Casting to a desired runtime type                        
            LglsAssumeDev(self.IsTyped(), "Block is not typed");
            const auto indirections = self.GetIndirections();

            if (indirections == IndirectsOf<TH>) {
               // No difference in indirections                         
               return *static_cast<THP>(heap);
            }
            else if (indirections > IndirectsOf<TH>) {
               // We need to dereference                                
               auto diff = indirections - IndirectsOf<TH>;
               Deep<C> denser = Disown(self.GetDense(diff));
               return *static_cast<THP>(denser.GetHeapInner());
            }
            else LglsError("Too many indirections");
         }
         else {
            // Casting to a desired static type                         
            if constexpr (IndirectsOf<TC> == IndirectsOf<TH>) {
               // No difference in indirections                         
               return *static_cast<THP>(static_cast<TCP>(heap));
            }
            else if constexpr (IndirectsOf<TC> > IndirectsOf<TH>) {
               // We need to dereference. Can be done without a         
               // reinterpret_cast, and thus be constexpr-friendly      
               return *static_cast<THP>(DenseCast<IndirectsOf<TC> - IndirectsOf<TH>>(static_cast<TCP>(heap)));
            }
            else static_assert(false, "Too many indirections");
         }
      }

      /// Get Nth element as a handle, or any desired wrapping type           
      ///   @tparam AS the type we're wrapping in                             
      ///   @param idx the index                                              
      ///   @return the element, as a reference if possible                   
      template<CT::NotVoid AS, CT::Container C>
      decltype(auto) AsAt(this C&& self, CT::Index auto&& idx) assumptious {
         static_assert(not CT::Reference<AS>, "Strip references first");

         if constexpr (CT::Handle<AS>) {
            if constexpr (CT::TypeErased<AS>) {
               // Type-erased handle                                    
               if constexpr (CT::DeeplyOwned<AS>)
                  return AS {self.GetAt(LglsFwd(idx)), self.GetEntries(), self.GetType()};
               else if constexpr (CT::Owned<AS>)
                  return AS {self.GetAt(LglsFwd(idx)), self.GetAllocation(), self.GetType()};
               else
                  return AS {self.GetAt(LglsFwd(idx)), self.GetType()};
            }
            else {
               // Statically typed handle                               
               using HT = Deref<TypeOf<AS>>;
               if constexpr (CT::TypeErased<C>) {
                  LglsAssert(self.template IsSame<HT>(), "Type mismatch",
                     ": ", self.GetType(), " not same as ", MetaDataOf<HT>());
               }
               else static_assert(Same<TypeOf<C>, HT>, "Type mismatch");

               if constexpr (CT::DeeplyOwned<AS>)
                  return AS {&self.GetAt(LglsFwd(idx)), self.GetEntries()};
               else if constexpr (CT::Owned<AS>)
                  return AS {&self.GetAt(LglsFwd(idx)), self.GetAllocation()};
               else
                  return AS {&self.GetAt(LglsFwd(idx))};
            }
         }
         else {
            // Access directly                                          
            if constexpr (CT::TypeErased<C>) {
               LglsAssert(self.template Is<AS>(), "Type mismatch",
                  ": ", self.GetType(), " not akin to ", MetaDataOf<AS>());
            }
            else static_assert(Akin<TypeOf<C>, AS>, "Type mismatch");
            return self.template GetAt<AS>(LglsFwd(idx));
         }
      }

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      auto CastAt(this C const&, CT::Index auto) -> AS;

      template<CT::Container C>
      auto GetItemAt(this C&&, CT::Index auto) assumptious -> Deep<C>;

      template<CT::Container C>
      auto GetItemAtDeep(this C&&, CT::Index auto) assumptious-> Deep<C>;

      template<CT::Container C>
      auto GetDeepAt(this C&&, CT::Index auto) assumptious-> Deep<C>*;

      template<CT::Container C>
      auto GetIndexMode(this C const&, Count<C>&) assumptious -> Count<C>;

      template<CT::Container C>
      auto Select(this C&&, CT::Index auto, Count<C>) assumptious -> PickRange<C>;

      template<CT::Container C>
      void SwapIndices(this C&, CT::Index auto, CT::Index auto) assumptious;
   };
}