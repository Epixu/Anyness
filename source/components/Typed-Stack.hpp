///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include "../states/Typed.hpp"
#include <Langulus/MetaOf.hpp>
#include <Langulus/CT/Akin.hpp>
#include <Langulus/CT/Deep.hpp>


namespace Langulus::Anyness
{
   using DMeta = RTTI::DMeta;
   using TMeta = RTTI::TMeta;
}

namespace Langulus::Anyness::Component
{
   /// Refers back to this particular component instance through the deduced  
   /// 'this'. Just for convenience. It is #undef-ed at the end of this file. 
   #define ThisCom self.TypedStack<META, TYPE, CONSTRAIN, ID>

   ///                                                                        
   /// Defines the contained type as a member variable, allowing the use of   
   /// type-erasure. You can optionally constrain the type at runtime.        
   ///   @tparam META the type of the meta                                    
   ///   @tparam TYPE optionally static type, use void for type-erasure       
   ///   @tparam CONSTRAIN override type-constraint                           
   ///   @tparam ID data provider that gets typed                             
   template<class META, class TYPE, bool CONSTRAIN, Cid ID>
   struct TypedStack : State::Typed<CONSTRAIN or CT::NotVoid<TYPE> ? StateValue::Enabled : StateValue::Variable, ID> {
      using CTTI_Component = Yes<>;
      using CTTI_Typed     = TYPE;
      using CTTI_ReflectAs = void;
      using StackRequest   = META;
      using Id             = Values<ID>;

      static constexpr int  ComponentPrecedence = -3000;
      static constexpr bool TypeErased = CT::Void<TYPE>;
      /// @attention valid only if not TypeErased                             
      static constexpr bool Sparse = not TypeErased and CT::Sparse<TYPE>;
      /// @attention valid only if not TypeErased                             
      static constexpr bool Dense  = not TypeErased and CT::Dense<TYPE>;

      /// MARK: Public                                                        
      /// Get the contained type - not possible at compile-time yet           
      ///   @tparam SID - type selector                                       
      template<Cid SID = ID> requires (SID == ID)
      constexpr META GetType(this auto const& self) noexcept {
         if consteval { return META {}; }
         else {
            META const& meta = ThisCom::GetTypeInner();
            if constexpr (not TypeErased)
               const_cast<META&>(meta) = MetaDataOf<TYPE>();
            return meta;
         }
      }

      /// Get the size of a single element in bytes                           
      ///   @tparam SID - type selector                                       
      template<Cid SID = ID> requires (SID == ID)
      constexpr size_t GetStride(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return ThisCom::GetTypeInner().GetSize();
         else
            return sizeof(TYPE);
      }

      /// Get the alignment of a single element in bytes                      
      ///   @tparam SID - type selector                                       
      template<Cid SID = ID> requires (SID == ID)
      constexpr pot_t GetAlignment(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return ThisCom::GetTypeInner().GetAlignment();
         else
            return pot_t(alignof(TYPE));
      }

      /// Get the reflected type name                                         
      ///   @tparam SID - type selector                                       
      template<Cid SID = ID> requires (SID == ID)
      constexpr auto GetName(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return ThisCom::GetTypeInner().GetName();
         else
            return NameOf<TYPE>();
      }

      /// Check if block has a data type                                      
      ///   @tparam SID - type selector                                       
      ///   @return true if data contained in this pack is specified          
      template<Cid SID = ID> requires (SID == ID)
      constexpr bool IsTyped(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return static_cast<bool>(ThisCom::GetTypeInner());
         else
            return true;
      }

      /// Check if type is akin to the provided type (can run at compile-time 
      /// if container is statically-typed)                                   
      ///   @attention ignores all sparsity and cv-qualifiers                 
      ///   @tparam T the type to compare against                             
      ///   @return true if origin types match                                
      template<CT::NotVoid T, Cid SID = ID> requires (SID == ID)
      constexpr bool Is(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return ThisCom::GetTypeInner().Is(MetaDataOf<T>());
         else
            return Akin<TYPE, T>;
      }

      /// Check if type origin is the same as another (always at runtime)     
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param type the type to check for                                 
      ///   @return true if this container's type is akin to 'type'           
      template<Cid SID = ID> requires (SID == ID)
      bool Is(this auto const& self, META type) noexcept {
         return ThisCom::GetTypeInner().Is(type);
      }

      /// Check if type origin is the same as another container's type        
      ///   @attention ignores sparsity and cv-qualifiers                     
      ///   @param other the type to check for                                
      ///   @return true if this container's type is akin to other's          
      template<Cid SID = ID, CT::Container C> requires (SID == ID)
      constexpr bool Is(this auto const& self, C const& other) noexcept {
         if constexpr (TypeErased or CT::TypeErased<C>)
            return ThisCom::GetTypeInner().Is(other.template GetType<ID>());
         else
            return Akin<TYPE, TypeOf<C>>;
      }

      /// Check if unqualified type is the same as provided one               
      ///   @attention ignores only cv-qualifiers (across all indirections)   
      ///   @tparam T the type to compare against                             
      ///   @return true if contained type is same as T                       
      template<CT::NotVoid T, Cid SID = ID> requires (SID == ID)
      constexpr bool IsSame(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return ThisCom::GetTypeInner().IsSame(MetaDataOf<T>());
         else
            return Same<TYPE, T>;
      }

      /// Check if unqualified type is the same as another                    
      ///   @attention ignores only cv-qualifiers                             
      ///   @param type the type to check for                                 
      ///   @return true if this block contains similar data                  
      template<Cid SID = ID> requires (SID == ID)
      bool IsSame(this auto const& self, META type) noexcept {
         return ThisCom::GetTypeInner().IsSame(type);
      }

      /// Check if unqualified type is the same as another container's type   
      ///   @attention ignores only cv-qualifiers                             
      ///   @param other the container to check for                           
      ///   @return true if this container has similar data                   
      template<Cid SID = ID, CT::Container C> requires (SID == ID)
      constexpr bool IsSame(this auto const& self, C const& other) noexcept {
         if constexpr (TypeErased or CT::TypeErased<C>)
            return ThisCom::GetTypeInner().IsSame(other.template GetType<ID>());
         else
            return Same<TYPE, TypeOf<C>>;
      }

      /// Check if this type is exactly T (references are ignored)            
      ///   @tparam T the type to compare against                             
      ///   @return true if data type matches T                               
      template<CT::NotVoid T, Cid SID = ID> requires (SID == ID)
      constexpr bool IsExact(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return ThisCom::GetTypeInner().IsExact(MetaDataOf<T>());
         else
            return Exact<TYPE, T>;
      }

      /// Check if this type is exactly another                               
      ///   @param type the type to match                                     
      ///   @return true if data type matches type exactly                    
      template<Cid SID = ID> requires (SID == ID)
      bool IsExact(this auto const& self, META type) noexcept {
         return ThisCom::GetTypeInner().IsExact(type);
      }

      /// Check if this type is exactly another container's type              
      ///   @param other the block to match                                   
      ///   @return true if data type matches type exactly                    
      template<Cid SID = ID, CT::Container C> requires (SID == ID)
      constexpr bool IsExact(this auto const& self, C const& other) noexcept {
         if constexpr (TypeErased or CT::TypeErased<C>)
            return ThisCom::GetTypeInner().IsExact(other.template GetType<ID>());
         else
            return Exact<TYPE, TypeOf<C>>;
      }
      
      /// Check if container contains pointers                                
      ///   @return true if the block contains pointers                       
      template<Cid SID = ID> requires (SID == ID)
      constexpr bool IsSparse(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return ThisCom::GetTypeInner().IsSparse();
         else
            return CT::Sparse<TYPE>;
      }
      
      /// Get the number of indirections                                      
      /// int**** will result in 4; int* will result in 1, int results in 0.  
      template<Cid SID = ID> requires (SID == ID)
      constexpr size_t GetIndirections(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return ThisCom::GetTypeInner().GetIndirections();
         else
            return IndirectsOf<TYPE>;
      }
      
      /// Check if block is constant                                          
      ///   @attention disowned containers are always constant                
      ///   @return true if the contents are constant                         
      template<Cid SID = ID> requires (SID == ID)
      constexpr bool IsConstant(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return self.IsDisowned() or ThisCom::GetTypeInner().IsConstant();
         else
            return CT::Constant<TYPE> or self.IsDisowned();
      }

      /// Check if container is made of other containers                      
      ///   @return true if the container is deep                             
      template<Cid SID = ID> requires (SID == ID)
      constexpr bool IsDeep(this auto const& self) noexcept {
         if constexpr (TypeErased)
            return ThisCom::GetTypeInner().IsDeep();
         else
            return CT::Deep<TYPE>;
      }

      /// Check if container contains executable items                        
      ///   @return true if the container has at least one executable element 
      template<Cid SID = ID, CT::Container C> requires (SID == ID)
      constexpr bool IsExecutable(this C const& self) noexcept {
         if (self.template IsEmpty<ID>())
            return false;

         if constexpr (TypeErased) {
            // Type-erased                                              
            const auto T = ThisCom::GetTypeInner();
            if (T.IsExecutable())
               return true;
            else if (T.IsDeep()) {
               bool result = false;
               self.Apply([&result](auto const& item) noexcept {
                  if (item.template Get<typename C::DeepType const, SID>()->template IsExecutable<ID>()) {
                     result = true;
                     return false;
                  }
                  else return true;
               });
               return result;

               /*if constexpr (CT::ContainsMany<C>) {
                  bool result = false;
                  self.ForEach([&result](typename C::DeepType const& inner) noexcept {
                     if (inner.template IsExecutable<ID>()) {
                        result = true;
                        return Loop::Break;
                     }
                     return Loop::Continue;
                  });
                  return result;
               }
               else return self.template As<typename C::DeepType const>().template IsExecutable<ID>();*/
            }
            else return false;
         }
         else {
            // Statically-typed                                         
            if constexpr (CT::Executable<TYPE>)
               return true;
            else if constexpr (CT::Deep<TYPE>) {
               bool result = false;
               self.Apply([&result](auto const& item) noexcept {
                  if (item.template Get<Decay<TYPE>, SID>()->template IsExecutable<ID>()) {
                     result = true;
                     return false;
                  }
                  else return true;
               });
               return result;
            }
            else return false;
         }
      }

      /// Get the size of the type times the contained elements               
      ///   @return the size of all elements in bytes                         
      template<Cid SID = ID> requires (SID == ID)
      constexpr size_t GetBytesize(this auto const& self) noexcept {
         return ThisCom::GetStride() * self.template GetCount<ID>();
      }

      /*template<bool BINARY_COMPATIBLE = false, bool ADVANCED = false>
      bool CastsToMeta(META) const;
      template<bool BINARY_COMPATIBLE = false>
      bool CastsToMeta(META, size_t) const;

      template<CT::NotVoid, bool BINARY_COMPATIBLE = false, bool ADVANCED = false>
      bool CastsTo() const;
      template<CT::NotVoid, bool BINARY_COMPATIBLE = false>
      bool CastsTo(size_t) const;*/

      /// Dereference the first element inside the container                  
      constexpr auto& operator * (this auto&& self) assumptious
      requires (not TypeErased and requires { *self.template GetRawAs<TYPE>(); }) {
         LglsAssumeDev(not self.IsEmpty(), "Container is empty");
         return *self.template GetRawAs<TYPE>();
      }

      /// Access the first element inside the container                       
      constexpr auto* operator -> (this auto&& self) assumptious
      requires (not TypeErased and requires { self.template GetRawAs<TYPE>(); }) {
         LglsAssumeDev(not self.IsEmpty(), "Container is empty");
         return self.template GetRawAs<TYPE>();
      }
      
      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      ///   @tparam T the new type                                            
      template<CT::NotVoid T, Cid SID = ID, CT::Container C> requires (SID == ID)
      void SetType(this C& self) {
         static_assert(CT::NotSheddable<T>, "Strip all sheddables first");
         static_assert(CT::NotReference<T>, "Strip all references first");
         
         const auto type = MetaDataOf<T>();
         if constexpr (TypeErased)
            ThisCom::SetType(type);
         else {
            static_assert(Exact<T, TYPE>, "Type mismatch");         
            ThisCom::SetTypeInner(type);
         }
      }

      /// Set the contained data type if possible.                            
      /// This is still used if statically typed - checks if types are        
      /// compatible in constructors and assigners.                           
      /// This particular override doesn't benefit from compile-time checks.  
      ///   @param type the new type                                          
      template<Cid SID = ID, CT::Container C> requires (SID == ID)
      void SetType(this C& self, META type) {
         auto& t = ThisCom::GetTypeInner();
         
         if constexpr (TypeErased) {
            // This container is type-erased                            
            if (t == type)
               return;
         
            if (not t) {
               t = type;
               return;
            }

            LglsAssert(not ThisCom::IsTypeConstrained(),
               "Attempting to mutate type-locked container"
               " of type ", t, " to type ", type
            );

            /*if (t.CastsTo(type)) {
               // Type is compatible, but only sparse data can mutate   
               // freely. Dense containers can't mutate because their   
               // destructors might be wrong later                      
               LglsAssert(t.IsSparse(), "Can't mutate ", t,
                  " to incompatible type ", type);
            }
            else {*/
               // Type is not compatible, but container is not typed, so
               // if it has no constructed elements we can still mutate 
               LglsAssert(self.template IsEmpty<ID>(), "Can't mutate ", t,
                  " to incompatible type ", type);
            /*}*/
            
            t = type;
         }
         else {
            // This container is statically typed                       
            if (not t)
               t = MetaDataOf<TYPE>();

            LglsAssert(t.IsExact(type), "Type mismatch", ": ", t,
               " is not exactly ", type);
         }
      }

      /// Deduce type of the container from provided argument                 
      ///   @param a The argument. Accepts intents, handles, arrays etc.      
      template<class A>
      constexpr void DeduceType(this auto& self, A const& a) {
         static_assert(not Same<A, Describe>,
            "Can't deduce type from a describe intent. "
            "You have to set it up manually.");

         if constexpr (CT::Handle<A>) {
            if constexpr (CT::TypeErased<A>)
               ThisCom::SetType(DeintCast(a).GetType());
            else
               ThisCom::template SetType<TypeOf<Deint<A>>>();
         }
         else ThisCom::template SetType<Decvq<DeextAll<Deref<Deint<A>>>>>();
      }

   protected:
      /// MARK: Protected                                                     
      LglsComRemoval(friend);
      LglsComHeapMovable(friend);
      LglsComIndexedCommon(friend);
      LglsComEmplacement(friend);

      /// Reset the type of the container, unless it's type-constrained.      
      /// If this container isn't type-erased, this call is a no-op.          
      ///   @attention allocation remains the same, and might not correspond  
      ///      to the next type which is set                                  
      template<Cid SID = ID> requires (SID == ID)
      constexpr void ResetType(this auto& self) noexcept {
         if constexpr (TypeErased) {
            if constexpr (requires { self.template IsTypeConstrained<SID>(); }) {
               if (not self.template IsTypeConstrained<SID>())
                  ThisCom::SetTypeInner({});
            }
            else ThisCom::SetTypeInner({});
         }
      }
      
      /// Resets all types, in case container is not Multitype                
      constexpr void ResetAllTypes(this auto& self) noexcept {
         ThisCom::ResetType();
      }
      
      /// Get the contained type (inner)                                      
      template<Cid SID = ID> requires (SID == ID)
      constexpr auto& GetTypeInner(this auto&& self) noexcept {
         return self.template AccessStack<TypedStack>();
      }

      /// Set the contained type (inner)                                      
      template<Cid SID = ID> requires (SID == ID)
      constexpr void SetTypeInner(this auto& self, const META& type) noexcept {
         ThisCom::GetTypeInner() = type;
      }

      /// Transfer from any kind of container, respecting intents.            
      /// Do it for a particular dimension.                                   
      ///   @param intent The intent and container to transfer from.          
      template<Cid D, class SELF, CT::Intent I> requires CT::Container<I>
      void SliceFrom(this SELF& self, I&& intent) {
         static_assert(CT::Disowned<I>);
         if constexpr (TypeErased) {
            ThisCom::SetType(intent->template GetType<D>());

            // While we are interfacing external memory, we have to     
            // keep the type-constrained state, otherwise we risk       
            // interpreting static memory the wrong way.                
            if constexpr (not CONSTRAIN) {
               if constexpr (not CT::TypeErased<I>)
                  // From statically-typed to dynamically-typed         
                  ThisCom::EnableTypeConstrained();
               else if (intent->template IsTypeConstrained<D>())
                  // From dynamically-typed to dynamically-typed        
                  ThisCom::EnableTypeConstrained();
            }
         }
         else {
            // These are called just to do compile-time type safety     
            if constexpr (CT::TypeErased<I>)
               ThisCom::SetType(intent->template GetType<D>());
            else
               ThisCom::template SetType<TypeOf<Deint<I>, D>>();
         }
      }

      /// Transfer from any kind of container, respecting intents             
      ///   @param intent the intent and container to transfer from           
      template<class SELF, CT::Intent I> requires CT::Container<I>
      void ConstructFrom(this SELF& self, I&& intent) {
         if constexpr (TypeErased) {
            ThisCom::SetType(intent->template GetType<ID>());

            // While we are interfacing external memory, we have to     
            // keep the type-constrained state, otherwise we risk       
            // interpreting static memory the wrong way.                
            if constexpr (not CONSTRAIN) {
               if constexpr (not CT::TypeErased<I>)
                  // From statically-typed to dynamically-typed         
                  ThisCom::EnableTypeConstrained();
               else if (intent->template IsTypeConstrained<ID>())
                  // From dynamically-typed to dynamically-typed        
                  ThisCom::EnableTypeConstrained();
            }
         }
         else {
            // These are called just to do compile-time type safety     
            if constexpr (CT::TypeErased<I>)
               ThisCom::SetType(intent->template GetType<ID>());
            else
               ThisCom::template SetType<TypeOf<Deint<I>, ID>>();
         }

         if constexpr (CT::Moved<I> and CT::TypeErased<I>)
            intent->template SetTypeInner<ID>(META{});
      }
   };

   #undef ThisCom
}
