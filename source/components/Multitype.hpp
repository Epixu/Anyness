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
   /// Combines multiple type components into a unified interface to combat   
   /// C++ base method ambiguities.                                           
   ///   @tparam TC... all the type components to unify                       
   template<CT::Component...TC> requires (sizeof...(TC) > 1)
   struct LANGULUS_EBCO Multitype : TC... {
      using CTTI_Component = Yes<>;
      using CTTI_Typed     = Types<TypeOf<TC>...>;
      using CTTI_ReflectAs = void;
      using Id             = decltype(((typename TC::Id{}) + ...));
      using Subcomponents  = Types<TC...>;

      static constexpr int ComponentPrecedence = -3000;
      static_assert(((TC::ComponentPrecedence == -3000) and ...),
       "All precedences should match");

      static constexpr bool TypeErased = (TC::TypeErased or ...);
      static_assert(((TC::TypeErased == TypeErased) and ...),
         "Currently all types must either be type-erased or not");

      /// Get the contained type                                              
      ///   @tparam SID - type selector                                       
      template<Cid SID>
      constexpr auto GetType(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::GetType();
            else
               return No {};
         });
      }

      /// Get the size of a single element in bytes                           
      ///   @tparam SID - type selector                                       
      template<Cid SID>
      constexpr size_t GetStride(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::GetStride();
            else
               return No {};
         });
      }

      /// Get the alignment of a single element in bytes                      
      ///   @tparam SID - type selector                                       
      template<Cid SID>
      constexpr pot_t GetAlignment(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::GetAlignment();
            else
               return No {};
         });
      }

      /// Get the reflected type name                                         
      ///   @tparam SID - type selector                                       
      template<Cid SID>
      constexpr auto GetName(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::GetName();
            else
               return No {};
         });
      }

      /// Check if block has a data type                                      
      ///   @tparam SID - type selector                                       
      ///   @return true if data contained in this pack is specified          
      template<Cid SID>
      constexpr bool IsTyped(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::IsTyped();
            else
               return No {};
         });
      }

      /// Check if type is akin to the provided type (can run at compile-time 
      /// if container is statically-typed)                                   
      ///   @attention ignores all sparsity and cv-qualifiers                 
      ///   @tparam T the type to compare against                             
      ///   @return true if origin types match                                
      template<CT::NotVoid T, Cid SID>
      constexpr bool Is(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::template Is<T>();
            else
               return No {};
         });
      }

      /// Check if type origin is the same as another (always at runtime)     
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param type the type to check for                                 
      ///   @return true if this container's type is akin to 'type'           
      template<Cid SID>
      bool Is(this auto const& self, auto&& type) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::Is(type);
            else
               return No {};
         });
      }

      /// Check if type origin is the same as another container's type        
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param other the type to check for                                
      ///   @return true if this container's type is akin to other's          
      template<Cid SID>
      constexpr bool Is(this auto const& self, CT::Container auto const& other) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::Is(other);
            else
               return No {};
         });
      }

      /// Check if unqualified type is the same as provided one               
      ///   @attention ignores only cv-qualifiers (across all indirections)   
      ///   @tparam T the type to compare against                             
      ///   @return true if contained type is same as T                       
      template<CT::NotVoid T, Cid SID>
      constexpr bool IsSame(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::template IsSame<T>();
            else
               return No {};
         });
      }

      /// Check if unqualified type is the same as another                    
      ///   @attention ignores only cv-qualifiers                             
      ///   @param type the type to check for                                 
      ///   @return true if this block contains similar data                  
      template<Cid SID>
      bool IsSame(this auto const& self, auto&& type) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::IsSame(type);
            else
               return No {};
         });
      }

      /// Check if unqualified type is the same as another container's type   
      ///   @attention ignores only cv-qualifiers                             
      ///   @param other the container to check for                           
      ///   @return true if this container has similar data                   
      template<Cid SID>
      constexpr bool IsSame(this auto const& self, CT::Container auto const& other) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::IsSame(other);
            else
               return No {};
         });
      }

      /// Check if this type is exactly T (references are ignored)            
      ///   @tparam T the type to compare against                             
      ///   @return true if data type matches T                               
      template<CT::NotVoid T, Cid SID>
      constexpr bool IsExact(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::template IsExact<T>();
            else
               return No {};
         });
      }

      /// Check if this type is exactly another                               
      ///   @param type the type to match                                     
      ///   @return true if data type matches type exactly                    
      template<Cid SID>
      bool IsExact(this auto const& self, auto&& type) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::IsExact(type);
            else
               return No {};
         });
      }

      /// Check if this type is exactly another container's type              
      ///   @param other the block to match                                   
      ///   @return true if data type matches type exactly                    
      template<Cid SID>
      constexpr bool IsExact(this auto const& self, CT::Container auto const& other) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::IsExact(other);
            else
               return No {};
         });
      }
      
      /// Check if container contains pointers                                
      ///   @return true if the block contains pointers                       
      template<Cid SID>
      constexpr bool IsSparse(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::IsSparse();
            else
               return No {};
         });
      }
      
      /// Get the number of indirections                                      
      /// int**** will result in 4; int* will result in 1, int results in 0.  
      template<Cid SID>
      constexpr size_t GetIndirections(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::GetIndirections();
            else
               return No {};
         });
      }
      
      /// Check if block is constant                                          
      ///   @attention disowned containers are always constant                
      ///   @return true if the contents are constant                         
      template<Cid SID>
      constexpr bool IsConstant(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::IsConstant();
            else
               return No {};
         });
      }

      /// Check if container is made of other containers                      
      ///   @return true if the container is deep                             
      template<Cid SID>
      constexpr bool IsDeep(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::IsDeep();
            else
               return No {};
         });
      }

      /// Check if container contains executable items                        
      ///   @return true if the container has at least one executable element 
      template<Cid SID>
      constexpr bool IsExecutable(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::IsExecutable();
            else
               return No {};
         });
      }

      /// Get the size of the type times the contained elements               
      ///   @return the size of all elements in bytes                         
      template<Cid SID>
      constexpr size_t GetBytesize(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::GetBytesize();
            else
               return No {};
         });
      }
      
      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      ///   @tparam T the new type                                            
      template<CT::NotVoid T, Cid SID>
      void SetType(this auto& self) {
         Subcomponents::ForEachConstOr([&]<class C> {
            if constexpr (C::Id::template Contains<SID>) {
               self.C::template SetType<T>();
               return true;
            }
            else return No {};
         });
      }

      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      /// This particular override doesn't benefit from compile-time checks.  
      ///   @param type the new type                                          
      template<Cid SID>
      void SetType(this auto& self, auto&& type) {
         Subcomponents::ForEachConstOr([&]<class C> {
            if constexpr (C::Id::template Contains<SID>) {
               self.C::SetType(type);
               return true;
            }
            else return No {};
         });
      }
      
   protected:
      LglsComRemoval(friend);
      LglsComHeapMovable(friend);
      LglsComIndexedCommon(friend);
      LglsComEmplacement(friend);

      /// Reset the type of the container, unless it's type-constrained.      
      /// If this container isn't type-erased, this call is a no-op.          
      ///   @attention allocation remains the same, and might not correspond  
      ///      to the next type which is set                                  
      template<Cid SID>
      constexpr void ResetType(this auto& self) noexcept {
         Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>) {
               self.C::ResetType();
               return true;
            }
            else return No {};
         });
      }
      
      /// Get the contained type (inner)                                      
      template<Cid SID>
      constexpr auto& GetTypeInner(this auto&& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept -> auto& {
            if constexpr (C::Id::template Contains<SID>)
               return self.C::GetTypeInner();
            else
               return No {};
         });
      }

      /// Set the contained type (inner)                                      
      template<Cid SID>
      constexpr void SetTypeInner(this auto& self, auto&& type) noexcept {
         Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::template Contains<SID>) {
               self.C::SetTypeInner(type);
               return true;
            }
            else return No {};
         });
      }

      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) {
         Subcomponents::ForEach([&]<class C> {
            self.C::ConstructFrom(LglsFwd(intent));
         });
      }
   };
}
