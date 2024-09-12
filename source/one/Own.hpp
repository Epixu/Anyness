///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "../Config.hpp"


namespace Langulus
{
   namespace A
   {

      ///                                                                     
      /// An abstract owned value                                             
      ///                                                                     
      struct Owned {
         LANGULUS(ABSTRACT) true;
         static constexpr bool CTTI_Container = true;
      };

   } // namespace Langulus::A

   namespace CT
   {

      /// Anything derived from A::Owned                                      
      template<class...T>
      concept Owned = ((CT::Dense<T> and DerivedFrom<T, A::Owned>) and ...);

      /// Anything not derived from A::Owned                                  
      template<class...T>
      concept NotOwned = ((not Owned<T>) and ...);

      /// Any owned pointer                                                   
      template<class...T>
      concept Pointer = Owned<T...> and Sparse<TypeOf<T>...>;

      /// Anything usable to initialize a shared pointer                      
      template<class...T>
      concept PointerRelated = ((Pointer<T> or Sparse<T> or Nullptr<T>) and ...);

   } // namespace Langulus::CT
   

   /// Owned == Owned                                                         
   template<CT::Owned T1, CT::Owned T2>
   requires CT::Comparable<TypeOf<T1>, TypeOf<T2>> LANGULUS(INLINED)
   constexpr bool operator == (const T1& lhs, const T2& rhs) noexcept {
      return lhs.Get() == rhs.Get();
   }

   /// Owned == Non-container type                                            
   template<CT::Owned T1, CT::NotContainer T2>
   requires CT::Comparable<TypeOf<T1>, T2> LANGULUS(INLINED)
   constexpr bool operator == (const T1& lhs, const T2& rhs) noexcept {
      return lhs.Get() == rhs;
   }

   /// Non-container type == Owned                                            
   template<CT::Owned T1, CT::NotContainer T2>
   requires CT::Comparable<T2, TypeOf<T1>> LANGULUS(INLINED)
   constexpr bool operator == (const T2& lhs, const T1& rhs) noexcept {
      return lhs == rhs.Get();
   }

} // namespace Langulus

namespace Langulus::Anyness
{

   ///                                                                        
   ///   An owned value, dense or sparse                                      
   ///                                                                        
   /// Provides ownership and semantics, for when you need to cleanup after a 
   /// move, for example. By default, fundamental types are not reset after a 
   /// move - wrapping them inside this ensures they are.                     
   ///   @attention this container is suboptimal for pointers, because it     
   ///              will constantly search the allocation corresponding to    
   ///              them, as it doesn't cache it in order to minimize size.   
   ///              For pointers, use Ptr or Ref instead. This doesn't really 
   ///              matter, if built without the MANAGED_MEMORY feature       
   ///                                                                        
   template<CT::Data T>
   class Own : public A::Owned {
   protected:
      T mValue;

   public:
      LANGULUS(NULLIFIABLE) CT::Nullifiable<T>;
      LANGULUS(POD) not CT::Sparse<T> and CT::POD<T>;
      LANGULUS(ABSTRACT) false;
      LANGULUS(TYPED) T;

      static constexpr bool Ownership = true;

      ///                                                                     
      ///   Construction                                                      
      ///                                                                     
      constexpr Own() requires CT::Defaultable<T>;
      constexpr Own(const Own&) requires (CT::Sparse<T> or CT::ReferMakable<T>);
      constexpr Own(Own&&) requires (CT::Sparse<T> or CT::MoveMakable<T>);

      template<template<class> class S>
      requires CT::SemanticMakable<S, T>
      constexpr Own(S<Own>&&);

      template<CT::NotOwned...A>
      requires ::std::constructible_from<T, A...>
      constexpr Own(A&&...);

      ///                                                                     
      ///   Assignment                                                        
      ///                                                                     
      constexpr Own& operator = (const Own&) requires (CT::Sparse<T> or CT::ReferAssignable<T>);
      constexpr Own& operator = (Own&&) requires (CT::Sparse<T> or CT::MoveAssignable<T>);

      template<template<class> class S>
      requires CT::SemanticAssignable<S, T>
      constexpr Own& operator = (S<Own>&&);

      template<CT::NotOwned A> requires CT::AssignableFrom<T, A>
      constexpr Own& operator = (A&&);

      ///                                                                     
      ///   Capsulation                                                       
      ///                                                                     
      NOD() DMeta GetType() const;
      NOD() Hash  GetHash() const requires CT::Hashable<T>;
      NOD() constexpr T const& Get() const noexcept;
      NOD() constexpr T&       Get()       noexcept;

      template<class>
      NOD() auto As() const noexcept requires CT::Sparse<T>;

      NOD() constexpr auto operator -> () const;
      NOD() constexpr auto operator -> ();

      NOD() constexpr auto& operator * () const IF_UNSAFE(noexcept)
         requires (CT::Sparse<T> and not CT::Void<Decay<T>>);
      NOD() constexpr auto& operator * ()       IF_UNSAFE(noexcept)
         requires (CT::Sparse<T> and not CT::Void<Decay<T>>);

      NOD() auto GetHandle() const;

      /// Makes Own CT::Resolvable                                            
      NOD() constexpr Block<T> GetBlock() const;

      ///                                                                     
      ///   Services                                                          
      ///                                                                     
      void Reset();

      NOD() explicit constexpr operator bool() const noexcept;
      NOD() constexpr operator T&() const noexcept;
   };

} // namespace Langulus::Anyness

namespace fmt
{

   ///                                                                        
   /// Extend FMT to be capable of logging any owned values                   
   ///                                                                        
   template<Langulus::CT::Owned T>
   struct formatter<T> {
      template<class CONTEXT>
      constexpr auto parse(CONTEXT& ctx) {
         return ctx.begin();
      }

      template<class CONTEXT> LANGULUS(INLINED)
      auto format(T const& element, CONTEXT& ctx) const {
         using namespace Langulus;
         if constexpr (CT::Sparse<TypeOf<T>>) {
            if (element == nullptr) {
               const auto type = element.GetType();
               if (type)
                  return fmt::format_to(ctx.out(), "{}(null)", type);
               else
                  return fmt::format_to(ctx.out(), "null");
            }
            else return fmt::format_to(ctx.out(), "{}", *element.Get());
         }
         else return fmt::format_to(ctx.out(), "{}", element.Get());
      }
   };

} // namespace fmt