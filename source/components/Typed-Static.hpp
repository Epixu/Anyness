#pragma once
#include <Langulus/MetaOf.hpp>


namespace Langulus::Anyness::Component
{

   template<CT::NotVoid T>
   struct TypedStatic {
      using CTTI_Component = Yes;

      RTTI::DMeta GetType() const noexcept { return MetaDataOf<T>(); }
   };

} // namespace Langulus::Anyness::Component
