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
   ///                                                                        
   /// Adds a variable to a container                                         
   /// Increases the container's bytesize                                     
   ///   @tparam T type of the variable                                       
   ///   @tparam ID multiple variables are supported                          
   template<CT::NotVoid T, Cid ID>
   struct Stack {
      using CTTI_Component = Yes<>;
      using StackRequest   = T;
      
      static constexpr Cid Id = ID;
      static constexpr Cid StackProvider = ID;
      static constexpr int ComponentPrecedence = -2000;
      
   protected:
      /// Get the heap pointer (inner)                                        
      constexpr auto& GetStackInner(this auto&& self) noexcept {
         return self.template AccessStack<Stack>();
      }

      constexpr void SetStackInner(this auto& self, T&& data) noexcept {
         self.GetStackInner() = LglsFwd(data);
      }

   public:
      /// Get a direct access to the stack memory                             
      template<CT::Container C>
      constexpr auto GetRaw(this C&& self) noexcept {
         if constexpr (CT::Mutable<C>)
            return &self.GetStackInner();
         else
            return &self.GetStackInner();
      }

      /// Get a direct access to the stack memory as a different type         
      template<class ALT, CT::Container C>
      constexpr auto GetRawAs(this C&& self) noexcept {
         if constexpr (CT::Mutable<C>)
            return static_cast<ALT*      >(&self.GetStackInner());
         else
            return static_cast<ALT const*>(&self.GetStackInner());
      }

      /// Get a direct access to the stack memory's end                       
      template<CT::Container C>
      constexpr auto GetRawEnd(this C&& self) noexcept {
         if constexpr (CT::Typed<C>)
            return self.GetRaw() + self.GetCount();
         else
            return self.template GetRawAs<uint8_t>() + self.GetBytesize();
      }

      /// Get reference to first element as sparse or dense, depending on T.  
      /// This is a lower-level routine that does only sparseness checking.   
      /// No conversion or copying occurs, only pointer arithmetic.           
      ///   @attention assumes the container is typed                         
      ///   @attention assumes the container is allocated                     
      ///   @tparam ALT optional type override, use T if void                 
      template<class ALT = void, CT::Container C>
      constexpr decltype(auto) Get(this C&& self) assumptious {
         static_assert(not CT::Handle<ALT>,    "ALT can't be a handle");
         static_assert(not CT::Reference<ALT>, "Strip references first");
         using TC = T;
         using TH = Tif<CT::Void<ALT>, TC, ALT>;
         using THQ1 = LglsMutIf(C, TH* );
         using THQ2 = LglsMutIf(C, TH**);
         auto& mStack = self.GetStackInner();

         // Casting to a desired static type                            
         if constexpr (CT::Sparse<TC>) {
            if constexpr (CT::Dense<TH>)
               // Representing sparse as dense                          
               return **static_cast<THQ2>(mStack);
            else
               // Representing sparse as sparse                         
               return  *static_cast<THQ1>(mStack);
         }
         else {
            if constexpr (CT::Dense<TH>)
               // Representing dense as dense                           
               return *static_cast<THQ1>( mStack);
            else
               // Representing dense as sparse                          
               return static_cast<Deptr<THQ1>>(mStack);
         }
      }

      /// Get first element as a handle, or any desired wrapping type         
      ///   @tparam ALT the type we're wrapping in                            
      ///   @return ALT, either as a reference if possible, or as a value if  
      ///      an incompatible pointer arithmetic happened                    
      template<class ALT, CT::Container C>
      decltype(auto) As(this C&& self) assumptious {
         if constexpr (CT::Handle<ALT>) {
            static_assert(not CT::Reference<ALT>, "Strip references first");

            if constexpr (ALT::TypeErased) {
               // Type-erased handle                                    
               if constexpr (requires { ALT::Owned; }) {
                  if constexpr (ALT::Owned)
                     return ALT {self.Get(), self.GetEntries(), self.GetType()};
                  else
                     return ALT {self.Get(), self.GetType()};
               }
               else return ALT {self.Get(), self.GetType()};
            }
            else {
               // Statically typed handle                               
               using HT = Deref<TypeOf<ALT>>;
               static_assert(Same<T, HT>, "Sparseness mismatch");

               if constexpr (requires { ALT::Owned; }) {
                  if constexpr (ALT::Owned)
                     return ALT {self.Stack::template Get<HT*>(), self.GetAllocation()};
                  else
                     return ALT {self.Stack::template Get<HT*>()};
               }
               else return ALT {self.Stack::template Get<HT*>()};
            }
         }
         else return self.template Get<Deref<ALT>>();
      }

   protected:
      /// Default-initialize count to zero                                    
      constexpr void ConstructDefault(this auto& self) noexcept {
         self.SetStackInner({});
      }
   };
}
