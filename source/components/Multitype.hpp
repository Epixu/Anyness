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
   template<class...> struct Multitype;

   template<CT::Component...TN> requires (CountEnabled<TN...> == 0)
   struct Multitype<TN...> {
      using CTTI_Component = Yes<>;
      static constexpr bool SkipThisComponent = true;
   };

   ///                                                                        
   /// Combines multiple type components into a unified interface to combat   
   /// C++ base method ambiguities, and to add a bit more convenience.        
   ///   @tparam TC0, TC1, TCN... all the type components to unify            
   template<CT::Component...TN> requires (CountEnabled<TN...> >= 2)
   struct LANGULUS_EBCO Multitype<TN...> : TN... {
      using CTTI_Component = Yes<>;
      using CTTI_ReflectAs = void;
      using Subcomponents  = decltype( Types<TN...>::Discard([]<class C>{ return requires { C::SkipThisComponent; }; }));
      using Id             = decltype(Subcomponents::Extract([]<class C>{ return typename C::Id{}; }));
      using CTTI_Typed     = decltype(Subcomponents::Extract([]<class C>{ return Types<TypeOf<C>>{}; }));

      static_assert(Subcomponents::ForEachAnd([]<class C> { return C::Id::Count == 1; }),
         "Each subcomponent needs to be dedicated to their single dimension");

      static constexpr int ComponentPrecedence = -3000;
      static_assert(Subcomponents::ForEachAnd([]<class C> { return C::ComponentPrecedence == -3000; }),
         "All precedences should match");

      static constexpr bool TypeErased = Subcomponents::ForEachOr([]<class C> { return C::TypeErased; });
      static_assert(Subcomponents::ForEachAnd([]<class C> { return C::TypeErased == TypeErased; }),
         "Currently all types must either be type-erased or not");

      #define if_inherits(...) requires (Subcomponents::ForEachOr([&]<class C> { \
         return requires { self.C::__VA_ARGS__; }; }))

      /// Get the contained type                                              
      ///   @tparam SID - type selector                                       
      template<Cid SID = Id::First>
      constexpr auto GetType(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::GetType();
            else
               return No {};
         });
      }
      constexpr auto GetKeyType(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::GetType();
      }
      constexpr auto GetValType(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::GetType();
      }

      /// Get the size of a single element in bytes                           
      ///   @tparam SID - type selector                                       
      template<Cid SID = Id::First>
      constexpr size_t GetStride(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::GetStride();
            else
               return No {};
         });
      }
      constexpr size_t GetKeyStride(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::GetStride();
      }
      constexpr size_t GetValStride(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::GetStride();
      }

      /// Get the alignment of a single element in bytes                      
      ///   @tparam SID - type selector                                       
      template<Cid SID = Id::First>
      constexpr pot_t GetAlignment(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::GetAlignment();
            else
               return No {};
         });
      }
      constexpr pot_t GetKeyAlignment(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::GetAlignment();
      }
      constexpr pot_t GetValAlignment(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::GetAlignment();
      }

      /// Get the reflected type name                                         
      ///   @tparam SID - type selector                                       
      template<Cid SID = Id::First>
      constexpr auto GetName(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::GetName();
            else
               return No {};
         });
      }
      constexpr auto GetKeyName(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::GetName();
      }
      constexpr auto GetValName(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::GetName();
      }

      /// Check if block has a data type                                      
      ///   @tparam SID - type selector                                       
      ///   @return true if data contained in this pack is specified          
      template<Cid SID = Id::First>
      constexpr bool IsTyped(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsTyped();
            else
               return No {};
         });
      }
      constexpr bool IsKeyTyped(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsTyped();
      }
      constexpr bool IsValTyped(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsTyped();
      }

      /// Check if type is akin to the provided type (can run at compile-time 
      /// if container is statically-typed)                                   
      ///   @attention ignores all sparsity and cv-qualifiers                 
      ///   @tparam T the type to compare against                             
      ///   @return true if origin types match                                
      template<CT::NotVoid T, Cid SID = Id::First>
      constexpr bool Is(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::template Is<T>();
            else
               return No {};
         });
      }
      template<CT::NotVoid T>
      constexpr bool IsKey(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::template Is<T>();
      }
      template<CT::NotVoid T>
      constexpr bool IsVal(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::template Is<T>();
      }

      /// Check if type origin is the same as another (always at runtime)     
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param type the type to check for                                 
      ///   @return true if this container's type is akin to 'type'           
      template<Cid SID = Id::First>
      bool Is(this auto const& self, auto const& type) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::Is(type);
            else
               return No {};
         });
      }
      constexpr bool IsKey(this auto const& self, auto const& type) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::Is(type);
      }
      constexpr bool IsVal(this auto const& self, auto const& type) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::Is(type);
      }

      /// Check if type origin is the same as another container's type        
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param other the type to check for                                
      ///   @return true if this container's type is akin to other's          
      template<Cid SID = Id::First>
      constexpr bool Is(this auto const& self, CT::Container auto const& other) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::Is(other);
            else
               return No {};
         });
      }
      constexpr bool IsKey(this auto const& self, CT::Container auto const& other) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::Is(other);
      }
      constexpr bool IsVal(this auto const& self, CT::Container auto const& other) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::Is(other);
      }

      /// Check if unqualified type is the same as provided one               
      ///   @attention ignores only cv-qualifiers (across all indirections)   
      ///   @tparam T the type to compare against                             
      ///   @return true if contained type is same as T                       
      template<CT::NotVoid T, Cid SID = Id::First>
      constexpr bool IsSame(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::template IsSame<T>();
            else
               return No {};
         });
      }
      template<CT::NotVoid T>
      constexpr bool IsKeySame(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::template IsSame<T>();
      }
      template<CT::NotVoid T>
      constexpr bool IsValSame(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::template IsSame<T>();
      }

      /// Check if unqualified type is the same as another                    
      ///   @attention ignores only cv-qualifiers                             
      ///   @param type the type to check for                                 
      ///   @return true if this block contains similar data                  
      template<Cid SID = Id::First>
      bool IsSame(this auto const& self, auto const& type) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsSame(type);
            else
               return No {};
         });
      }
      constexpr bool IsKeySame(this auto const& self, auto const& type) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsSame(type);
      }
      constexpr bool IsValSame(this auto const& self, auto const& type) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsSame(type);
      }

      /// Check if unqualified type is the same as another container's type   
      ///   @attention ignores only cv-qualifiers                             
      ///   @param other the container to check for                           
      ///   @return true if this container has similar data                   
      template<Cid SID = Id::First>
      constexpr bool IsSame(this auto const& self, CT::Container auto const& other) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsSame(other);
            else
               return No {};
         });
      }
      constexpr bool IsKeySame(this auto const& self, CT::Container auto const& type) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsSame(type);
      }
      constexpr bool IsValSame(this auto const& self, CT::Container auto const& type) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsSame(type);
      }

      /// Check if this type is exactly T (references are ignored)            
      ///   @tparam T the type to compare against                             
      ///   @return true if data type matches T                               
      template<CT::NotVoid T, Cid SID = Id::First>
      constexpr bool IsExact(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::template IsExact<T>();
            else
               return No {};
         });
      }
      template<CT::NotVoid T>
      constexpr bool IsKeyExact(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::template IsExact<T>();
      }
      template<CT::NotVoid T>
      constexpr bool IsValExact(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::template IsExact<T>();
      }

      /// Check if this type is exactly another                               
      ///   @param type the type to match                                     
      ///   @return true if data type matches type exactly                    
      template<Cid SID = Id::First>
      bool IsExact(this auto const& self, auto&& type) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsExact(type);
            else
               return No {};
         });
      }
      constexpr bool IsKeyExact(this auto const& self, auto const& type) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsExact(type);
      }
      constexpr bool IsValExact(this auto const& self, auto const& type) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsExact(type);
      }

      /// Check if this type is exactly another container's type              
      ///   @param other the block to match                                   
      ///   @return true if data type matches type exactly                    
      template<Cid SID = Id::First>
      constexpr bool IsExact(this auto const& self, CT::Container auto const& other) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsExact(other);
            else
               return No {};
         });
      }
      constexpr bool IsKeyExact(this auto const& self, CT::Container auto const& type) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsExact(type);
      }
      constexpr bool IsValExact(this auto const& self, CT::Container auto const& type) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsExact(type);
      }

      /// Check if container contains pointers                                
      ///   @return true if the block contains pointers                       
      template<Cid SID = Id::First>
      constexpr bool IsSparse(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsSparse();
            else
               return No {};
         });
      }
      constexpr bool IsKeySparse(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsSparse();
      }
      constexpr bool IsValSparse(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsSparse();
      }

      /// Get the number of indirections                                      
      /// int**** will result in 4; int* will result in 1, int results in 0.  
      template<Cid SID = Id::First>
      constexpr size_t GetIndirections(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::GetIndirections();
            else
               return No {};
         });
      }
      constexpr size_t GetKeyIndirections(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::GetIndirections();
      }
      constexpr size_t GetValIndirections(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::GetIndirections();
      }

      /// Check if block is constant                                          
      ///   @attention disowned containers are always constant                
      ///   @return true if the contents are constant                         
      template<Cid SID = Id::First>
      constexpr bool IsConstant(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsConstant();
            else
               return No {};
         });
      }
      constexpr bool IsKeyConstant(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsConstant();
      }
      constexpr bool IsValConstant(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsConstant();
      }

      /// Check if container is made of other containers                      
      ///   @return true if the container is deep                             
      template<Cid SID = Id::First>
      constexpr bool IsDeep(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsDeep();
            else
               return No {};
         });
      }
      constexpr bool IsKeyDeep(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsDeep();
      }
      constexpr bool IsValDeep(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsDeep();
      }

      /// Check if container contains executable items                        
      ///   @return true if the container has at least one executable element 
      template<Cid SID = Id::First>
      constexpr bool IsExecutable(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsExecutable();
            else
               return No {};
         });
      }
      constexpr bool IsKeyExecutable(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsExecutable();
      }
      constexpr bool IsValExecutable(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsExecutable();
      }

      /// Get the size of the type times the contained elements               
      ///   @return the size of all elements in bytes                         
      template<Cid SID = Id::First>
      constexpr size_t GetBytesize(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::GetBytesize();
            else
               return No {};
         });
      }
      constexpr size_t GetKeyBytesize(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::GetBytesize();
      }
      constexpr size_t GetValBytesize(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::GetBytesize();
      }

      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      ///   @tparam T the new type                                            
      template<CT::NotVoid T, Cid SID = Id::First>
      void SetType(this auto& self) {
         Subcomponents::ForEachConstOr([&]<class C> {
            if constexpr (C::Id::First == SID) {
               self.C::template SetType<T>();
               return true;
            }
            else return No {};
         });
      }
      template<CT::NotVoid T>
      constexpr void SetKeyType(this auto const& self) requires (Id::First == 0) {
         self.Subcomponents::First::template SetType<T>();
      }
      template<CT::NotVoid T>
      constexpr void SetValType(this auto const& self) requires (Id::Second == 1) {
         self.Subcomponents::Second::template SetType<T>();
      }

      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      /// This particular override doesn't benefit from compile-time checks.  
      ///   @param type the new type                                          
      template<Cid SID = Id::First>
      void SetType(this auto& self, auto const& type) {
         Subcomponents::ForEachConstOr([&]<class C> {
            if constexpr (C::Id::First == SID) {
               self.C::SetType(type);
               return true;
            }
            else return No {};
         });
      }
      constexpr void SetKeyType(this auto const& self, auto const& type) requires (Id::First == 0) {
         self.Subcomponents::First::SetType(type);
      }
      constexpr void SetValType(this auto const& self, auto const& type) requires (Id::Second == 1) {
         self.Subcomponents::Second::SetType(type);
      }

      /// Check if type is mutable when the container is empty                
      template<Cid SID = Id::First>
      constexpr bool IsTypeConstrained(this auto const& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID)
               return self.C::IsTypeConstrained();
            else
               return No {};
         });
      }
      constexpr bool IsKeyTypeConstrained(this auto const& self) noexcept requires (Id::First == 0) {
         return self.Subcomponents::First::IsTypeConstrained();
      }
      constexpr bool IsValTypeConstrained(this auto const& self) noexcept requires (Id::Second == 1) {
         return self.Subcomponents::Second::IsTypeConstrained();
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
      template<Cid SID = Id::First>
      constexpr void ResetType(this auto& self) noexcept {
         Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID) {
               self.C::ResetType();
               return true;
            }
            else return No {};
         });
      }
      
      /// Get the contained type (inner)                                      
      template<Cid SID = Id::First>
      constexpr auto& GetTypeInner(this auto&& self) noexcept {
         return Subcomponents::ForEachConstOr([&]<class C> noexcept -> auto& {
            if constexpr (C::Id::First == SID)
               return self.C::GetTypeInner();
            else
               return No {};
         });
      }

      /// Set the contained type (inner)                                      
      template<Cid SID = Id::First>
      constexpr void SetTypeInner(this auto& self, auto&& type) noexcept {
         Subcomponents::ForEachConstOr([&]<class C> noexcept {
            if constexpr (C::Id::First == SID) {
               self.C::SetTypeInner(type);
               return true;
            }
            else return No {};
         });
      }

      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      template<CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this auto& self, I&& intent) if_inherits(ConstructFrom(LglsFwd(intent))) {
         Subcomponents::ForEach([&]<class C> {
            if_available(self.C::ConstructFrom(LglsFwd(intent)));
         });
      }

      #undef if_inherits
   };
}
