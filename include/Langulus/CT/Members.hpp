#pragma once
#include "../NameOf.hpp"


namespace Langulus
{
   namespace Inner
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
         static constexpr Literal Name = NameOf<HANDLE>();

         constexpr MemberReflector() = default;
         constexpr MemberReflector(Emballage<HANDLE>, TYPE OWNER::*) {}
      };

      /// This CTAD auto-completes the additional template arguments          
      /// No harm done :)                                                     
      template<auto HANDLE, class OWNER, class TYPE>
      MemberReflector(Emballage<HANDLE>, TYPE OWNER::*) -> MemberReflector<HANDLE, OWNER, TYPE>;

   } // namespace Langulus::Inner


   /// Can be used to reflect named members inside your T like so:            
   /// public: using CTTI_Members = Members<&T::mOne, &T::mTwo>;              
   template<auto...M>
   struct Members {
      using List = Types<
         decltype(Inner::MemberReflector(Fake<Inner::Emballage<M>>(), M))...
      >;
   };

} // namespace Langulus
