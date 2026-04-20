///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.Stack<T, ID>

   ///                                                                        
   /// Adds a variable to a container. Supports references.                   
   /// Increases the container's bytesize.                                    
   ///   @tparam T type of the variable (supports references)                 
   ///   @tparam ID the stack provider ID for use with other components.      
   ///      All stack/heap providers must have unique sequential IDs.         
   template<CT::NotVoid T, Cid ID>
   struct Stack {
      using CTTI_Component = Yes<>;
      using StackRequest   = T;
      
      static constexpr Cid Id = ID;
      static constexpr Cid StackProvider = ID;
      static constexpr int ComponentPrecedence = -2000;
      
      /// Get a direct access to the stack memory                             
      template<Cid SID = ID, CT::Container C> requires (SID == ID)
      constexpr auto GetRaw(this C&& self) noexcept {
         return &ThisCom::GetStackInner();
      }

      /// Get a direct access to the stack memory as a different type         
      template<class AS, Cid SID = ID, CT::Container C> requires (SID == ID)
      constexpr auto GetRawAs(this C&& self) noexcept {
         using AScvq = LglsMutIf(C, AS*);
         return static_cast<AScvq>(ThisCom::GetRaw());
      }

      /// Get a direct access to the stack memory's end                       
      template<Cid SID = ID, CT::Container C> requires (SID == ID)
      constexpr auto GetRawEnd(this C&& self) noexcept {
         return ThisCom::GetRaw() + 1;
      }

      /// Get a direct access to the stack memory's end                       
      template<Cid SID = ID, CT::Container C> requires (SID == ID)
      constexpr auto GetRawReserveEnd(this C&& self) noexcept {
         return ThisCom::GetRawEnd();
      }

      /// Get reference to first element as sparse or dense, depending on T.  
      /// This is a lower-level routine that does only sparseness checking.   
      /// No conversion or copying occurs, only pointer arithmetic.           
      ///   @attention no type-safety                                         
      ///   @tparam AS the type of data we're accessing - use void to use the 
      ///      type of the stack                                              
      template<class AS = void, Cid SID = ID, CT::Container C> requires (SID == ID)
      constexpr decltype(auto) Get(this C&& self) assumptious {
         static_assert(not CT::Handle<AS>,    "AS can't be a handle");
         static_assert(not CT::Reference<AS>, "Strip references first");

         using TC    = LglsMutIf(C, Deref<T>);
         using TCP   = LglsMutIf(C, TC*);
         using TH    = Tif<CT::Void<AS>, TC, AS>;
         using THP   = LglsMutIf(C, TH*);
         auto& stack = ThisCom::GetStackInner();

         // Casting to a desired static type                            
         if constexpr (IndirectsOf<TC> == IndirectsOf<TH>) {
            // No difference in indirections                            
            return *static_cast<THP>(static_cast<TCP>(&stack));
         }
         else if constexpr (IndirectsOf<TC> > IndirectsOf<TH>) {
            // We need to dereference. Can be done without a            
            // reinterpret_cast, and thus be constexpr-friendly.        
            // Supports packed pointers as well.                        
            return *static_cast<THP>(DenseCast<IndirectsOf<TC> - IndirectsOf<TH>>(static_cast<TCP>(&stack)));
         }
         else {
            // We are allowed to add one additional indirection         
            static_assert(IndirectsOf<TCP> == IndirectsOf<TH>,
               "Too many indirections");
            return *const_cast<THP>(reinterpret_cast<ConstAll<THP>>(&stack));
         }
      }

      /// Get first element as a handle, or any desired wrapping type.        
      /// Conversion or copying may occur, depending on type.                 
      ///   @tparam AS the type we're wrapping in                             
      ///   @return the element, as a reference if possible                   
      template<CT::NotVoid AS, Cid SID = ID, CT::Container C>
      requires (SID == ID and CT::Contiguous<C>)
      decltype(auto) As(this C&& self) {
         static_assert(not CT::Reference<AS>, "Strip references first");

         if constexpr (CT::Handle<AS>)
            return self.template GetHandle<AS>();
         else {
            // Access directly or wrapped in a container                
            if constexpr (Akin<T, AS>) {
               // Access directly                                       
               return ThisCom::template Get<AS>();
            }
            else if constexpr (CT::DeepDense<AS>) {
               // Wrap in a container                                   
               Decvq<AS> temp {Absorb, self};
               if_available(temp.SetCountInner(1));
               return temp;
            }
            else static_assert(false, "Type mismatch");
         }
      }

      /// A safe way to get the first sparse entry after being resolved to    
      /// the most concrete type. Available only if container has DeepType.   
      ///   @return the most concrete representation of the first item        
      template<class AS = void, Cid SID = ID, CT::Container C>
      requires (SID == ID and CT::Contiguous<C> and requires { typename Deref<C>::DeepType; })
      auto GetResolved(this C&& self) {
         using D = Tif<CT::Void<AS>, typename Deref<C>::DeepType, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         static_assert(CT::HasVariableCount<D>, "D must allow for being empty");

         if (self.IsEmpty())
            return D {};
         if (not self.IsSparse())
            return ThisCom::template As<D>();

         if constexpr (CT::Resolvable<T>)
            return D {DenseCast(ThisCom::Get()).GetResolved()};
         else
            return D {DenseCast(ThisCom::Get())};
      }

      /// Get the first contained element, removing 'count' indirections.     
      /// Available only if container has DeepType defined.                   
      ///   @attention throws if type is incomplete and origin was reached    
      ///   @tparam AS specify the type we wrap the result in.                
      ///      Using 'void' will choose C::DeepType.                          
      ///   @param self deduced this                                          
      ///   @param count how many levels of indirection to remove?            
      ///   @return the dense first element                                   
      template<class AS = void, Cid SID = ID, CT::Container C>
      requires (SID == ID and CT::Contiguous<C> and requires { typename Deref<C>::DeepType; })
      auto GetDense(this C&& self, size_t count = -1) {
         using D = Tif<CT::Void<AS>, typename Deref<C>::DeepType, AS>;
         static_assert(CT::Container<D>, "D must result in a container type");
         LglsAssert(not self.IsEmpty(), "Can't GetDense from empty container");
         if (not self.IsSparse() or count <= 0)
            return D {Absorb, Disown(self)};

         // Check if origin type is complete before attempting anything 
         if (count >= IndirectsOf<T>) {
            LglsAssert(CT::Complete<Decay<T>>,
               "Trying to interface incomplete data `", self.GetType(),
               "` as dense"
            );
         }

         void* src = DecvqAllCast(&ThisCom::GetStackInner());
         auto type = self.GetType();
         while (count and type.IsSparse()) {
            auto nextType = type.GetDeptr();
            
            if (nextType.IsSparse()) {
               // Pointer T -> Pointer nextT                            
               type.GetDereffer()(src, &src);
            }
            else {
               // Pointer T -> Dense nextT                              
               D temp {Absorb, Disown(self)};
               temp.SetTypeInner(nextType);
               temp.SetHeapInner(UnpackPointer(type, nextType, src));
               if_available(temp.SetCountInner(1));
               return temp;
            }

            type = nextType;
            --count;
         }
         
         LglsError("Should never be reached");
         return D {Absorb, Disown(self)};
      }

   protected:
      /// Default-initialize the variable                                     
      constexpr void ConstructDefault(this auto& self) noexcept requires CT::NotReference<T> {
         ThisCom::SetStackInner({});
      }

      /// Get the heap pointer (inner)                                        
      constexpr auto& GetStackInner(this auto&& self) noexcept {
         return self.template AccessStack<Stack>();
      }

      /// Set the heap pointer (inner)                                        
      constexpr void SetStackInner(this auto& self, T&& data) noexcept {
         ThisCom::GetStackInner() = LglsFwd(data);
      }
   };

   #undef ThisCom
}
