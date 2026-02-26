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
#include <Langulus/Allocator.hpp>


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Provides a common element access interface.                            
   /// Needs to be specialized, relying on a custom SimplifyIndex method.     
   ///   @tparam ID the stack/heap we're indexing                             
   template<Cid ID>
   struct IndexedCommon {
      using CTTI_Component = Yes<>;

      static constexpr bool Indexed = true;
      static constexpr int  ComponentPrecedence = 3000;

   protected:
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;

      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

   public:
      /// Subscript operator for accessing element at a specific index        
      ///   @param idx the index                                              
      ///   @return the picked element                                        
      template<CT::Container C>
      decltype(auto) operator[] (this C&& self, CT::Index auto&& idx) assumptious {
         if constexpr (CT::TypeErased<C>)
            return self.template AsAt<DecidePick<C>>(LglsFwd(idx));
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
      CT::Sparse auto GetAt(this C&& self, CT::Index auto&& idx) assumptious {
         static_assert(not CT::Handle<AS>,    "AS can't be a handle");
         static_assert(not CT::Reference<AS>, "Strip references first");
         using TC  = TypeOf<C>*;
         using TCP = LglsMutIf(C, TC);
         using TH  = Tif<CT::Void<AS>, TC, AS>;
         //using TH  = Tif<CT::Void<AS>, TC, Tif<CT::Sparse<AS>, AS, AS*>>;
         //using THS = Tif<CT::Sparse<TH>, TH, TH*>;
         //using THP = LglsMutIf(C, THS);
         using THP = LglsMutIf(C, TH);

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
               const     auto tci = self.GetIndirections() + 1;
               constexpr auto thi = IndirectsOf<TH>;

               if (tci == thi) {
                  // No difference in indirections                      
                  return static_cast<THP>(static_cast<TCP>(heap));
               }
               else if (tci > thi) {
                  // We need to dereference. Supports packed pointers.  
                  auto diff = tci - thi;
                  Deep<C> denser = Disown(self.GetDenseAt(idx, diff)); //TODO does a redundant offset
                  return static_cast<THP>(denser.GetHeapInner());
               }
               else {
                  // We are allowed to add one additional indirection   
                  LglsAssumeDev(tci + 1 == thi, "Too many indirections");
                  return static_cast<THP>(static_cast<TCP>(heap));
               }
            }
         }
         else {
            // Casting to a desired static type                         
            constexpr auto tci = IndirectsOf<TC/*TypeOf<C>*/>;
            constexpr auto thi = IndirectsOf<TH>;

            if constexpr (tci == thi) {
               // No difference in indirections                         
               return static_cast<THP>(static_cast<TCP>(heap));
            }
            else if constexpr (tci > thi) {
               // We need to dereference. Can be done without a         
               // reinterpret_cast, and thus be constexpr-friendly.     
               // Supports packed pointers as well.                     
               return static_cast<THP>(DenseCast<tci - thi>(static_cast<TCP>(heap)));
            }
            else {
               // We are allowed to add one additional indirection      
               static_assert(tci + 1 == thi, "Too many indirections");
               return static_cast<THP>(static_cast<TCP>(heap));
            }
         }
      }

      /// Get Nth element as a handle, or any desired wrapping type.          
      /// Conversion or copying may occur, depending on type.                 
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
            // Access directly or wrapped in a container                
            if constexpr (CT::TypeErased<C>) {
               if (self.template Is<AS>()) {
                  // Access directly                                    
                  if constexpr (CT::Deep<AS> and CT::Dense<AS>)
                     return Decvq<AS> {Absorb, *self.template GetAt<AS*>(LglsFwd(idx))};
                  else if constexpr (CT::Dense<AS>)
                     return *self.template GetAt<AS*>(LglsFwd(idx));
                  else
                     return  self.template GetAt<AS >(LglsFwd(idx));
               }
               else if constexpr (CT::Deep<AS> and CT::Dense<AS>) {
                  // Wrap in a container                                
                  return Decvq<AS> {Absorb, self.template AsAt<DecideHandle<C>>(LglsFwd(idx))};
               }
               else {
                  // Runtime type mismatch error                        
                  LglsError("Type mismatch", ": ", self.GetType(),
                     " not akin to ", MetaDataOf<AS>());
                  if constexpr (CT::Dense<AS>)
                     return *self.template GetAt<AS*>(LglsFwd(idx));
                  else
                     return  self.template GetAt<AS >(LglsFwd(idx));
               }
            }
            else {
               if constexpr (Akin<TypeOf<C>, AS>) {
                  // Access directly                                    
                  if constexpr (CT::Dense<AS>)
                     return *self.template GetAt<AS*>(LglsFwd(idx));
                  else
                     return  self.template GetAt<AS >(LglsFwd(idx));
               }
               else if constexpr (CT::Deep<AS> and CT::Dense<AS>) {
                  // Wrap in a container                                
                  return Decvq<AS> {Absorb, self.template AsAt<DecideHandle<C>>(LglsFwd(idx))};
               }
               else static_assert(false, "Type mismatch");
            }
         }
      }
      
      /// Get Nth deep item using a deep index                                
      ///   @attention ignores sparseness                                     
      ///   @param idx the deep index                                         
      ///   @return a pointer to the first deep item, or nullptr if not deep  
      template<class AS = void, CT::Container C>
      auto GetDeepAt(this C&& self, CT::Index auto&&) noexcept {
         using D = Tif<CT::Void<AS>, LglsMutIf(C, Deep<C>*), LglsMutIf(C, AS*)>;
         if (self.IsEmpty() or not self.IsDeep())
            return D {nullptr};
         return self.template As<D>();
      }

      /// Get Nth element after being resolved to the most concrete type.     
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

      /// Get Nth element, removing 'count' indirections                      
      ///   @attention throws if type is incomplete and origin was reached    
      ///   @tparam AS specify the type we wrap the result in.                
      ///      Using 'void' will default to C::DeepType.                      
      ///   @param idx the index                                              
      ///   @param count how many levels of indirection to remove?            
      ///   @return the dense first element                                   
      template<class AS = void, CT::Container C>
      auto GetDenseAt(this C&& self, CT::Index auto&& idx, size_t count = -1) {
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
            D temp;
            temp.SetTypeInner(self.GetType());
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
               D temp;
               temp.SetTypeInner(nextT);
               temp.SetHeapInner(UnpackPointer(T, nextT, heap));
               if_available(temp.SetCountInner(1));
               return temp;
            }

            T = nextT;
            --count;
         }
         
         LglsError("Should never be reached");
         return D {};
      }

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      auto CastAt(this C const&, CT::Index auto&&) -> AS;
   };
}
