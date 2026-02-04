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
#include <Langulus/Allocator.hpp>
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
      template<unsigned, class>      friend struct Insertion;
      template<unsigned, CT::Sparse> friend struct HeapMovable;

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
         if constexpr (CT::TypeErased<C>)
            result.SetHeapInner(result.template GetRawAs<uint8_t>() + start * result.GetStride());
         else
            result.SetHeapInner(result.GetRaw() + start);
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
         if constexpr (CT::TypeErased<C>)
            return self.template AsAt<DecideHandle<C>>(LglsFwd(idx));
         else
            return *self.GetAt(LglsFwd(idx));
      }

      /// Get pointer to Nth element.                                         
      /// This is a lower-level routine that does only sparseness checking.   
      /// No conversion or copying occurs, only pointer arithmetic.           
      ///   @attention no type-safety                                         
      ///   @attention assumes the container is typed                         
      ///   @attention assumes the container has valid memory                 
      ///   @tparam AS the type of data we're accessing - use void to use the 
      ///      type of the container, if statically typed                     
      ///   @param idx the index                                              
      ///   @return the chosen element                                        
      template<class AS = void, CT::Container C>
      auto* GetAt(this C&& self, CT::Index auto&& idx) assumptious {
         static_assert(not CT::Handle<AS>,    "AS can't be a handle");
         static_assert(not CT::Reference<AS>, "Strip references first");
         using TC  = TypeOf<C>*;
         using TH  = Tif<CT::Void<AS>, TC, AS>;
         using TCP = LglsMutIf(C, TC);
         using THS = Tif<CT::Sparse<TH>, TH, TH*>;
         using THP = LglsMutIf(C, THS);

         // Offset to the proper heap pointer                           
         const auto offset = self.SimplifyIndex(idx);
         const auto byte_offset = self.GetStride() * offset;
         void* heap = DecvqAllCast(self.GetHeapInner());
         heap = reinterpret_cast<void*>(
            reinterpret_cast<uint8_t*>(heap) + byte_offset
         );

         // Dereference it if we have to                                
         if constexpr (CT::TypeErased<C>) {
            if constexpr (CT::Void<AS>) {
               // Unknown type, just return the offsetted heap pointer  
               return heap;
            }
            else {
               // Casting to a desired runtime type                     
               LglsAssumeDev(self.IsTyped(), "Block is not typed");
               const auto indirections = self.GetIndirections();

               if (indirections == IndirectsOf<TH>) {
                  // No difference in indirections                      
                  return static_cast<THP>(static_cast<TCP>(heap));
               }
               else if (indirections > IndirectsOf<TH>) {
                  // We need to dereference. Supports packed pointers.  
                  auto diff = indirections - IndirectsOf<TH>;
                  Deep<C> denser = Disown(self.GetDenseAt(idx, diff)); //TODO does a redundant offset
                  return static_cast<THP>(denser.GetHeapInner());
               }
               else {
                  // We are allowed to add one additional indirection   
                  LglsAssumeDev(indirections + 1 == IndirectsOf<TH>,
                     "Too many indirections");
                  return static_cast<THP>(static_cast<TCP>(heap));
               }
            }
         }
         else {
            // Casting to a desired static type                         
            constexpr auto indirections = IndirectsOf<TypeOf<C>>;

            if constexpr (indirections == IndirectsOf<TH>) {
               // No difference in indirections                         
               return static_cast<THP>(static_cast<TCP>(heap));
            }
            else if constexpr (indirections > IndirectsOf<TH>) {
               // We need to dereference. Can be done without a         
               // reinterpret_cast, and thus be constexpr-friendly.     
               // Supports packed pointers as well.                     
               return static_cast<THP>(DenseCast<indirections - IndirectsOf<TH>>(static_cast<TCP>(heap)));
            }
            else {
               // We are allowed to add one additional indirection      
               static_assert(indirections + 1 == IndirectsOf<TH>,
                  "Too many indirections");
               return static_cast<THP>(static_cast<TCP>(heap));
            }
         }
      }

      /// Get Nth element as a handle, any desired wrapping type or reference 
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
                  return AS {self.GetAt(LglsFwd(idx)), self.GetEntries()};
               else if constexpr (CT::Owned<AS>)
                  return AS {self.GetAt(LglsFwd(idx)), self.GetAllocation()};
               else
                  return AS {self.GetAt(LglsFwd(idx))};
            }
         }
         else {
            // Access directly                                          
            if constexpr (CT::TypeErased<C>) {
               LglsAssert(self.template Is<AS>(), "Type mismatch",
                  ": ", self.GetType(), " not akin to ", MetaDataOf<AS>());
            }
            else static_assert(Akin<TypeOf<C>, AS>, "Type mismatch");

            if constexpr (CT::Dense<AS>)
               return *self.template GetAt<AS*>(LglsFwd(idx));
            else
               return  self.template GetAt<AS>(LglsFwd(idx));
         }
      }
      
      /// A safe way to get the first deep entry                              
      ///   @attention ignores sparseness                                     
      ///   @param idx the index                                              
      ///   @return a pointer to the first deep item, or nullptr if not deep  
      template<class AS = void, CT::Container C>
      auto GetDeepAt(this C&& self, CT::Index auto&&) noexcept {
         using D = Tif<CT::Void<AS>, LglsMutIf(C, Deep<C>*), LglsMutIf(C, AS*)>;
         if (self.IsEmpty() or not self.IsDeep())
            return D {nullptr};
         return self.template As<D>();
      }

      /// A safe way to get the first sparse entry after being resolved to    
      /// the most concrete type.                                             
      ///   @param idx the index                                              
      ///   @return the most concrete representation of the first item        
      template<class AS = void, CT::Container C>
      auto GetResolvedAt(this C&& self, CT::Index auto&&) {
         using D = Tif<CT::Void<AS>, Deep<C>, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         static_assert(CT::HasVariableCount<D>, "D must allow for being empty");

         if (self.IsEmpty())
            return D {};
         if (not self.IsSparse())
            return self.template GetItem<D>();

         if constexpr (CT::TypeErased<C>) {
            const auto T = self.GetType();
            const auto resolver = T.GetResolver();
            if (resolver)
               return D {resolver(self.GetDense().GetRaw())};
            else
               return self.template GetDense<D>();

         }
         else {
            using T = TypeOf<C>;
            if constexpr (CT::Resolvable<T>)
               return D {DenseCast(self.template Get<T>()).GetResolved()};
            else
               return D {DenseCast(self.template Get<T>())};
         }
      }

      /// Get the first contained element, removing 'count' indirections      
      ///   @attention throws if type is incomplete and origin was reached    
      ///   @tparam AS specify the type we wrap the result in.                
      ///      Using 'void' will default to C::DeepType.                      
      ///   @param idx the index                                              
      ///   @param count how many levels of indirection to remove?            
      ///   @return the dense first element                                   
      template<class AS = void, CT::Container C>
      auto GetDenseAt(this C&& self, CT::Index auto&& idx, Count<C> count = CountMax<C>) {
         using D = Tif<CT::Void<AS>, Deep<C>, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         LglsAssert(not self.IsEmpty(), "Can't GetDense from empty container");

         // Offset the heap                                             
         void* heap = DecvqAllCast(self.GetHeapInner());
         const auto offset = self.SimplifyIndex(idx);
         const auto byte_offset = self.GetStride() * offset;
         heap = reinterpret_cast<void*>(
            reinterpret_cast<uint8_t*>(heap) + byte_offset
         );

         if (not self.IsSparse() or count <= 0) {
            // Early return if nothing to do                            
            D temp {Absorb, Disown(self)};
            temp.SetHeapInner(heap);
            if_available(temp.SetCountInner(1));
            return temp;
         }

         // Check if origin type is complete before attempting anything 
         if constexpr (CT::TypeErased<C>) {
            const auto T = self.GetType();
            if (count >= T.GetIndirections()) {
               LglsAssert(T.GetOrigin(),
                  "Trying to interface incomplete data `", self.GetType(),
                  "` as dense"
               );
            }
         }
         else {
            using T = TypeOf<C>;
            if (count >= IndirectsOf<T>) {
               LglsAssert(CT::Complete<Decay<T>>,
                  "Trying to interface incomplete data `", self.GetType(),
                  "` as dense"
               );
            }
         }

         auto T = self.GetType();
         while (count and T.IsSparse()) {
            auto nextT = T.GetDeptr();
            
            if (nextT.IsSparse()) {
               // Pointer T -> Pointer nextT                            
               T.GetDereffer()(heap, &heap);
            }
            else {
               // Pointer T -> Dense nextT                              
               D temp {Absorb, Disown(self)};
               temp.SetTypeInner(nextT);
               temp.SetHeapInner(UnpackPointer(T, nextT, heap));
               if_available(temp.SetCountInner(1));
               return temp;
            }

            T = nextT;
            --count;
         }
         
         LglsError("Should never be reached");
         return D {Absorb, Disown(self)};
      }

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      auto CastAt(this C const&, CT::Index auto&&) -> AS;

      /*template<CT::Container C>
      auto GetItemAt(this C&&, CT::Index auto) assumptious -> Deep<C>;

      template<CT::Container C>
      auto GetItemAtDeep(this C&&, CT::Index auto) assumptious-> Deep<C>;

      template<CT::Container C>
      auto GetDeepAt(this C&&, CT::Index auto) assumptious-> Deep<C>*;*/

      template<CT::Container C>
      auto GetIndexMode(this C const&, Count<C>&) assumptious -> Count<C>;

      template<CT::Container C>
      auto Select(this C&&, CT::Index auto&&, Count<C>) assumptious -> PickRange<C>;

      template<CT::Container C>
      void SwapIndices(this C&, CT::Index auto&&, CT::Index auto) assumptious;
   };
}