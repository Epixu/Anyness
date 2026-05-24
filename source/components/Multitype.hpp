///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
LglsDisableWarningPush
LglsDisableWarning_UnusedLocalTypedef


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
      using Subcomponents  = decltype( Types<TN...>::Discard([]<class C> static { return requires { C::SkipThisComponent; }; }));
      using Id             = decltype(Subcomponents::Extract([]<class C> static { return typename C::Id{}; }));
      using CTTI_Typed     = decltype(Subcomponents::Extract([]<class C> static { return Types<TypeOf<C>>{}; }));

      static_assert(Subcomponents::ForEachIndexedAnd([]<class C, size_t I> {
         return C::Id::Count == 1 and C::Id::First == I; }),
         "Each enabled subcomponent needs to be dedicated to their single dimension, "
         "and all subcomponents need to be sequential"
      );

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
      template<Cid SID = 0>
      constexpr auto GetType(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetType();
      }
      constexpr auto GetKeyType(this auto const& self) noexcept {
         return self.Subcomponents::First::GetType();
      }
      constexpr auto GetValType(this auto const& self) noexcept {
         return self.Subcomponents::Second::GetType();
      }

      /// Get the size of a single element in bytes                           
      ///   @tparam SID - type selector                                       
      template<Cid SID = 0>
      constexpr size_t GetStride(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetStride();
      }
      constexpr size_t GetKeyStride(this auto const& self) noexcept {
         return self.Subcomponents::First::GetStride();
      }
      constexpr size_t GetValStride(this auto const& self) noexcept {
         return self.Subcomponents::Second::GetStride();
      }

      /// Get the alignment of a single element in bytes                      
      ///   @tparam SID - type selector                                       
      template<Cid SID = 0>
      constexpr pot_t GetAlignment(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetAlignment();
      }
      constexpr pot_t GetKeyAlignment(this auto const& self) noexcept {
         return self.Subcomponents::First::GetAlignment();
      }
      constexpr pot_t GetValAlignment(this auto const& self) noexcept {
         return self.Subcomponents::Second::GetAlignment();
      }

      /// Get the reflected type name                                         
      ///   @tparam SID - type selector                                       
      template<Cid SID = 0>
      constexpr auto GetName(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetName();
      }
      constexpr auto GetKeyName(this auto const& self) noexcept {
         return self.Subcomponents::First::GetName();
      }
      constexpr auto GetValName(this auto const& self) noexcept {
         return self.Subcomponents::Second::GetName();
      }

      /// Check if block has a data type                                      
      ///   @tparam SID - type selector                                       
      ///   @return true if data contained in this pack is specified          
      template<Cid SID = 0>
      constexpr bool IsTyped(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsTyped();
      }
      constexpr bool IsKeyTyped(this auto const& self) noexcept {
         return self.Subcomponents::First::IsTyped();
      }
      constexpr bool IsValTyped(this auto const& self) noexcept {
         return self.Subcomponents::Second::IsTyped();
      }

      /// Check if type is akin to the provided type (can run at compile-time 
      /// if container is statically-typed)                                   
      ///   @attention ignores all sparsity and cv-qualifiers                 
      ///   @tparam T the type to compare against                             
      ///   @return true if origin types match                                
      template<CT::NotVoid T, Cid SID = 0>
      constexpr bool Is(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::template Is<T>();
      }
      template<CT::NotVoid T>
      constexpr bool IsKey(this auto const& self) noexcept {
         return self.Subcomponents::First::template Is<T>();
      }
      template<CT::NotVoid T>
      constexpr bool IsVal(this auto const& self) noexcept {
         return self.Subcomponents::Second::template Is<T>();
      }

      /// Check if type origin is the same as another (always at runtime)     
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param type the type to check for                                 
      ///   @return true if this container's type is akin to 'type'           
      template<Cid SID = 0>
      bool Is(this auto const& self, auto const& type) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::Is(type);
      }
      constexpr bool IsKey(this auto const& self, auto const& type) noexcept {
         return self.Subcomponents::First::Is(type);
      }
      constexpr bool IsVal(this auto const& self, auto const& type) noexcept {
         return self.Subcomponents::Second::Is(type);
      }

      /// Check if type origin is the same as another container's type        
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param other the type to check for                                
      ///   @return true if this container's type is akin to other's          
      template<Cid SID = 0>
      constexpr bool Is(this auto const& self, CT::Container auto const& other) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::Is(other);
      }
      constexpr bool IsKey(this auto const& self, CT::Container auto const& other) noexcept {
         return self.Subcomponents::First::Is(other);
      }
      constexpr bool IsVal(this auto const& self, CT::Container auto const& other) noexcept {
         return self.Subcomponents::Second::Is(other);
      }

      /// Check if unqualified type is the same as provided one               
      ///   @attention ignores only cv-qualifiers (across all indirections)   
      ///   @tparam T the type to compare against                             
      ///   @return true if contained type is same as T                       
      template<CT::NotVoid T, Cid SID = 0>
      constexpr bool IsSame(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::template IsSame<T>();
      }
      template<CT::NotVoid T>
      constexpr bool IsKeySame(this auto const& self) noexcept {
         return self.Subcomponents::First::template IsSame<T>();
      }
      template<CT::NotVoid T>
      constexpr bool IsValSame(this auto const& self) noexcept {
         return self.Subcomponents::Second::template IsSame<T>();
      }

      /// Check if unqualified type is the same as another                    
      ///   @attention ignores only cv-qualifiers                             
      ///   @param type the type to check for                                 
      ///   @return true if this block contains similar data                  
      template<Cid SID = 0>
      bool IsSame(this auto const& self, auto const& type) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsSame(type);
      }
      constexpr bool IsKeySame(this auto const& self, auto const& type) noexcept {
         return self.Subcomponents::First::IsSame(type);
      }
      constexpr bool IsValSame(this auto const& self, auto const& type) noexcept {
         return self.Subcomponents::Second::IsSame(type);
      }

      /// Check if unqualified type is the same as another container's type   
      ///   @attention ignores only cv-qualifiers                             
      ///   @param other the container to check for                           
      ///   @return true if this container has similar data                   
      template<Cid SID = 0>
      constexpr bool IsSame(this auto const& self, CT::Container auto const& other) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsSame(other);
      }
      constexpr bool IsKeySame(this auto const& self, CT::Container auto const& type) noexcept {
         return self.Subcomponents::First::IsSame(type);
      }
      constexpr bool IsValSame(this auto const& self, CT::Container auto const& type) noexcept {
         return self.Subcomponents::Second::IsSame(type);
      }

      /// Check if this type is exactly T (references are ignored)            
      ///   @tparam T the type to compare against                             
      ///   @return true if data type matches T                               
      template<CT::NotVoid T, Cid SID = 0>
      constexpr bool IsExact(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::template IsExact<T>();
      }
      template<CT::NotVoid T>
      constexpr bool IsKeyExact(this auto const& self) noexcept {
         return self.Subcomponents::First::template IsExact<T>();
      }
      template<CT::NotVoid T>
      constexpr bool IsValExact(this auto const& self) noexcept {
         return self.Subcomponents::Second::template IsExact<T>();
      }

      /// Check if this type is exactly another                               
      ///   @param type the type to match                                     
      ///   @return true if data type matches type exactly                    
      template<Cid SID = 0>
      bool IsExact(this auto const& self, auto&& type) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsExact(type);
      }
      constexpr bool IsKeyExact(this auto const& self, auto const& type) noexcept {
         return self.Subcomponents::First::IsExact(type);
      }
      constexpr bool IsValExact(this auto const& self, auto const& type) noexcept {
         return self.Subcomponents::Second::IsExact(type);
      }

      /// Check if this type is exactly another container's type              
      ///   @param other the block to match                                   
      ///   @return true if data type matches type exactly                    
      template<Cid SID = 0>
      constexpr bool IsExact(this auto const& self, CT::Container auto const& other) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsExact(other);
      }
      constexpr bool IsKeyExact(this auto const& self, CT::Container auto const& type) noexcept {
         return self.Subcomponents::First::IsExact(type);
      }
      constexpr bool IsValExact(this auto const& self, CT::Container auto const& type) noexcept {
         return self.Subcomponents::Second::IsExact(type);
      }

      /// Check if container contains pointers                                
      ///   @return true if the block contains pointers                       
      template<Cid SID = 0>
      constexpr bool IsSparse(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsSparse();
      }
      constexpr bool IsKeySparse(this auto const& self) noexcept {
         return self.Subcomponents::First::IsSparse();
      }
      constexpr bool IsValSparse(this auto const& self) noexcept {
         return self.Subcomponents::Second::IsSparse();
      }

      /// Get the number of indirections                                      
      /// int**** will result in 4; int* will result in 1, int results in 0.  
      template<Cid SID = 0>
      constexpr size_t GetIndirections(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetIndirections();
      }
      constexpr size_t GetKeyIndirections(this auto const& self) noexcept {
         return self.Subcomponents::First::GetIndirections();
      }
      constexpr size_t GetValIndirections(this auto const& self) noexcept {
         return self.Subcomponents::Second::GetIndirections();
      }

      /// Check if block is constant                                          
      ///   @attention disowned containers are always constant                
      ///   @return true if the contents are constant                         
      template<Cid SID = 0>
      constexpr bool IsConstant(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsConstant();
      }
      constexpr bool IsKeyConstant(this auto const& self) noexcept {
         return self.Subcomponents::First::IsConstant();
      }
      constexpr bool IsValConstant(this auto const& self) noexcept {
         return self.Subcomponents::Second::IsConstant();
      }

      /// Check if container is made of other containers                      
      ///   @return true if the container is deep                             
      template<Cid SID = 0>
      constexpr bool IsDeep(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsDeep();
      }
      constexpr bool IsKeyDeep(this auto const& self) noexcept {
         return self.Subcomponents::First::IsDeep();
      }
      constexpr bool IsValDeep(this auto const& self) noexcept {
         return self.Subcomponents::Second::IsDeep();
      }

      /// Check if container contains executable items                        
      ///   @return true if the container has at least one executable element 
      template<Cid SID = 0>
      constexpr bool IsExecutable(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsExecutable();
      }
      constexpr bool IsKeyExecutable(this auto const& self) noexcept {
         return self.Subcomponents::First::IsExecutable();
      }
      constexpr bool IsValExecutable(this auto const& self) noexcept {
         return self.Subcomponents::Second::IsExecutable();
      }

      /// Get the size of the type times the contained elements               
      ///   @return the size of all elements in bytes                         
      template<Cid SID = 0>
      constexpr size_t GetBytesize(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetBytesize();
      }
      constexpr size_t GetKeyBytesize(this auto const& self) noexcept {
         return self.Subcomponents::First::GetBytesize();
      }
      constexpr size_t GetValBytesize(this auto const& self) noexcept {
         return self.Subcomponents::Second::GetBytesize();
      }

      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      ///   @tparam T the new type                                            
      template<CT::NotVoid T, Cid SID = 0>
      void SetType(this auto& self) {
         using C = typename Subcomponents::template At<SID>;
         self.C::template SetType<T>();
      }
      template<CT::NotVoid T>
      constexpr void SetKeyType(this auto const& self) {
         self.Subcomponents::First::template SetType<T>();
      }
      template<CT::NotVoid T>
      constexpr void SetValType(this auto const& self) {
         self.Subcomponents::Second::template SetType<T>();
      }

      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      /// This particular override doesn't benefit from compile-time checks.  
      ///   @param type the new type                                          
      template<Cid SID = 0>
      void SetType(this auto& self, auto const& type) {
         using C = typename Subcomponents::template At<SID>;
         self.C::SetType(type);
      }
      constexpr void SetKeyType(this auto const& self, auto const& type) {
         self.Subcomponents::First::SetType(type);
      }
      constexpr void SetValType(this auto const& self, auto const& type) {
         self.Subcomponents::Second::SetType(type);
      }

      /// Check if type is mutable when the container is empty                
      template<Cid SID = 0>
      constexpr bool IsTypeConstrained(this auto const& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::IsTypeConstrained();
      }
      constexpr bool IsKeyTypeConstrained(this auto const& self) noexcept {
         return self.Subcomponents::First::IsTypeConstrained();
      }
      constexpr bool IsValTypeConstrained(this auto const& self) noexcept {
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
      template<Cid SID = 0>
      constexpr void ResetType(this auto& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         self.C::ResetType();
      }
      
      /// Get the contained type (inner)                                      
      template<Cid SID = 0>
      constexpr auto& GetTypeInner(this auto&& self) noexcept {
         using C = typename Subcomponents::template At<SID>;
         return self.C::GetTypeInner();
      }

      /// Set the contained type (inner)                                      
      template<Cid SID = 0>
      constexpr void SetTypeInner(this auto& self, auto&& type) noexcept {
         using C = typename Subcomponents::template At<SID>;
         self.C::SetTypeInner(type);
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

LglsDisableWarningPop
