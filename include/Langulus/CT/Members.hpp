///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../NameOf.hpp"


namespace Langulus::CTTI
{
   /// Can be used to reflect members in two ways:                            
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Members = Members<constants...>;` in T     
   template<class T>
   struct DefineMembers {
      using Type = void;
      static constexpr bool Enabled = false;
   };
}

namespace Langulus::CT::Inner
{
   template<auto T>
   struct Emballage {};

   /// Used for member reflections inside data types                          
   ///   @tparam HANDLE - a pointer to a member variable                      
   ///   @attention having just `auto HANDLE` as a template argument          
   ///      should generally be enough and works as it should on MSVC,        
   ///      however it doesn't make unique template instantiations on         
   ///      Clang and causes very nasty bugs. So, we're forced to add         
   ///      a couple more template parameters, to ensure proper templating    
   ///      We compensate by using CTAD to deduce those parameters            
   template<auto HANDLE, class OWNER, class TYPE>
   struct MemberReflector {
      using Member = decltype(HANDLE);
      static_assert(std::is_member_pointer_v<Member>,
         "Member must be a member pointer");
      using Owner = OWNER;
      using Type  = TYPE;
      static constexpr TYPE OWNER::*Handle = HANDLE;
      static constexpr auto Name = CppNameOf<HANDLE>();

      constexpr MemberReflector() = default;
      constexpr MemberReflector(Emballage<HANDLE>, TYPE OWNER::*) {}
   };

   /// This CTAD auto-completes the additional template arguments             
   template<auto HANDLE, class OWNER, class TYPE>
   MemberReflector(Emballage<HANDLE>, TYPE OWNER::*)
      -> MemberReflector<HANDLE, OWNER, TYPE>;

   /// Helper function to extract reflected members                           
   template<class T>
   consteval auto GetMembers() {
      static_assert(not ::std::is_reference_v<T>,
         "Strip references first");
      static_assert(not CT::Convoluted<T>,
         "Strip qualifiers first");

      if constexpr (CTTI::DefineMembers<T>::Enabled) {
         // Checked externally, T doesn't have to be complete           
         return typename CTTI::DefineMembers<T>::Type {};
      }
      else if constexpr (requires { typename T::CTTI_Members; }) {
         // Checked internally, T has to be a complete type             
         return typename T::CTTI_Members {};
      }
   };
}

namespace Langulus
{
   /// Can be used to reflect named members inside your T like so:            
   /// public: using CTTI_Members = Members<&T::mOne, &T::mTwo>;              
   /// Or by specializing CTTI::DefineMembers<T>                              
   template<auto...M>
   struct Members : Types<
         decltype(CT::Inner::MemberReflector(Fake<CT::Inner::Emballage<M>>(), M))...
   > {};

   /// Get the reflected named members, void if none                          
   template<class T>
   using MembersOf = decltype(CT::Inner::GetMembers<Decvq<Deref<T>>>());
}
