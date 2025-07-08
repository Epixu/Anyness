///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#include "Main.hpp"
#include <Langulus/MetaOf.hpp>

using namespace Langulus;

namespace
{
   class IncompleteType;

   template<class T>
   void TestDefinition() {
      auto meta = MetaDataOf<T>();
      REQUIRE(meta);
      REQUIRE(meta != nullptr);
      REQUIRE(meta.GetName() == NameOf<T>());
      REQUIRE(meta.GetVersionMajor() == VersionOf<T>().Major);
      REQUIRE(meta.GetVersionMinor() == VersionOf<T>().Minor);
      REQUIRE(meta.IsDeep() == CT::Deep<T>);
      REQUIRE(meta.IsPOD() == CT::POD<T>);
      REQUIRE(meta.IsNullable() == CT::Nullable<T>);
      IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolTactic() == CT::GetPoolTactic<T>()));
      REQUIRE(meta.GetConcrete() == nullptr);
      REQUIRE(meta.GetProducer() == nullptr);
      REQUIRE(meta.GetAllocationPage() == ::std::max(Alignment, Roof2(sizeof(T))));
      REQUIRE(meta.IsAbstract() == CT::Abstract<T>);
      REQUIRE(meta.GetSize() == sizeof(T));
      REQUIRE(meta.GetAlignment() == alignof(T));

      if constexpr (CT::Complete<Deptr<T>>)
         REQUIRE(meta.GetDeptr() == MetaDataOf<Deptr<T>>());
      else
         REQUIRE(meta.GetDeptr() == nullptr);
      
      if constexpr (CT::Complete<Decay<T>>)
         REQUIRE(meta.GetOrigin() == MetaDataOf<Decay<T>>());
      else
         REQUIRE(meta.GetOrigin() == nullptr);
            
      REQUIRE(meta.IsConstant() == false);

      if constexpr (CT::Complete<Decay<T>>)
         REQUIRE(meta.GetOrigin() == MetaDataOf<Decay<T>>());
      else
         REQUIRE(meta.GetOrigin() == nullptr);
      
      REQUIRE(meta.GetDecvqAll() == MetaDataOf<IncompleteType*>());

      REQUIRE(meta.GetBases().size() == 0);
      REQUIRE(meta.GetAbilities().size() == 0);
      REQUIRE(meta.GetMembers().size() == 0);
      REQUIRE(meta.GetNamedValues().size() == 0);
      REQUIRE(meta.GetMorphismsTo().size() == 0);
      REQUIRE(meta.GetMorphismsFrom().size() == 0);
   }
}

TEMPLATE_TEST_CASE("Testing reflection", "[rtti]",
   IncompleteType*
) {
   
}

SCENARIO("An incomplete type reflected (as long as its a pointer)", "[metadata]") {
   WHEN("IncompleteType* reflected") {
      auto meta = MetaDataOf<IncompleteType*>();

      REQUIRE(meta);
      REQUIRE(meta != nullptr);
      REQUIRE(meta.GetName() == "IncompleteType*");
      REQUIRE(meta.GetVersionMajor() == 1);
      REQUIRE(meta.GetVersionMinor() == 0);
      REQUIRE(meta.IsDeep() == false);
      REQUIRE(meta.IsPOD() == false);
      REQUIRE(meta.IsNullable() == false);
      IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta.GetPoolTactic() == PoolTactic::Default));
      REQUIRE(meta.GetConcrete() == nullptr);
      REQUIRE(meta.GetAllocationPage() == ::std::max(Alignment, Roof2(sizeof(IncompleteType*))));
      REQUIRE(meta.IsAbstract() == false);
      REQUIRE(meta.GetSize() == sizeof(void*));
      REQUIRE(meta.GetAlignment() == alignof(void*));
      REQUIRE(meta.GetDeptr() == nullptr);
      REQUIRE(meta.GetOrigin() == nullptr);
      REQUIRE(meta.IsConstant() == false);
      REQUIRE(meta.GetDecvqAll() == MetaDataOf<IncompleteType*>());

      REQUIRE(meta.GetBases().size() == 0);
      REQUIRE(meta.GetAbilities().size() == 0);
      REQUIRE(meta.GetMembers().size() == 0);
      REQUIRE(meta.GetNamedValues().size() == 0);
      REQUIRE(meta.GetMorphismsTo().size() == 0);
      REQUIRE(meta.GetMorphismsFrom().size() == 0);
   }

   WHEN("IncompleteType** reflected") {
      auto meta = MetaDataOf<IncompleteType**>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "IncompleteType**");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == false);
      REQUIRE(meta->mIsNullifiable == false);
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         REQUIRE(meta->mPoolTactic == PoolTactic::Default);
      #endif
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage == ::std::max(Alignment, Roof2(sizeof(IncompleteType*))));
      REQUIRE(meta->mIsAbstract == false);
      REQUIRE(meta->mSize == sizeof(void*));
      REQUIRE(meta->mAlignment == alignof(void*));
      REQUIRE(meta->mDeptr == MetaDataOf<IncompleteType*>());
      REQUIRE(meta->mOrigin == nullptr);
      REQUIRE(meta->mIsConstant == false);
      REQUIRE(meta->mDeptr->mIsConstant == false);
      REQUIRE(meta->mDeptr->mDeptr == nullptr);
      REQUIRE(meta->mDecvq == MetaDataOf<IncompleteType**>());

      REQUIRE(meta->mBases.size() == 0);
      REQUIRE(meta->mAbilities.size() == 0);
      REQUIRE(meta->mMembers.size() == 0);
      REQUIRE(meta->mNamedValues.size() == 0);
      REQUIRE(meta->mConvertersTo.size() == 0);
      REQUIRE(meta->mConvertersFrom.size() == 0);
   }

   WHEN("const IncompleteType** reflected") {
      auto meta = MetaDataOf<const IncompleteType**>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "const IncompleteType**");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == false);
      REQUIRE(meta->mIsNullifiable == false);
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         REQUIRE(meta->mPoolTactic == PoolTactic::Default);
      #endif
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage == ::std::max(Alignment, Roof2(sizeof(IncompleteType*))));
      REQUIRE(meta->mIsAbstract == false);
      REQUIRE(meta->mSize == sizeof(void*));
      REQUIRE(meta->mAlignment == alignof(void*));
      REQUIRE(meta->mDeptr == MetaDataOf<const IncompleteType*>());
      REQUIRE(meta->mOrigin == nullptr);
      REQUIRE(meta->mIsConstant == false);
      REQUIRE(meta->mDeptr->mIsConstant == false);
      REQUIRE(meta->mDeptr->mDeptr == nullptr);
      REQUIRE(meta->mDecvq == MetaDataOf<IncompleteType**>());

      REQUIRE(meta->mBases.size() == 0);
      REQUIRE(meta->mAbilities.size() == 0);
      REQUIRE(meta->mMembers.size() == 0);
      REQUIRE(meta->mNamedValues.size() == 0);
      REQUIRE(meta->mConvertersTo.size() == 0);
      REQUIRE(meta->mConvertersFrom.size() == 0);
   }

   WHEN("const IncompleteType* const*& reflected") {
      auto meta = MetaDataOf<const IncompleteType* const*&>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "const IncompleteType* const*");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == false);
      REQUIRE(meta->mIsNullifiable == false);
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         REQUIRE(meta->mPoolTactic == PoolTactic::Default);
      #endif
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage == ::std::max(Alignment, Roof2(sizeof(IncompleteType*))));
      REQUIRE(meta->mIsAbstract == false);
      REQUIRE(meta->mSize == sizeof(void*));
      REQUIRE(meta->mAlignment == alignof(void*));
      REQUIRE(meta->mDeptr == MetaDataOf<const IncompleteType* const>());
      REQUIRE(meta->mOrigin == nullptr);
      REQUIRE(meta->mIsConstant == false);
      REQUIRE(meta->mDeptr->mIsConstant == true);
      REQUIRE(meta->mDeptr->mDeptr == nullptr);
      REQUIRE(meta->mDecvq == MetaDataOf<IncompleteType**>());
      REQUIRE(meta->mDeptr->mDecvq == MetaDataOf<IncompleteType*>());

      REQUIRE(meta->mBases.size() == 0);
      REQUIRE(meta->mAbilities.size() == 0);
      REQUIRE(meta->mMembers.size() == 0);
      REQUIRE(meta->mNamedValues.size() == 0);
      REQUIRE(meta->mConvertersTo.size() == 0);
      REQUIRE(meta->mConvertersFrom.size() == 0);
   }

   WHEN("const IncompleteType* const* const& reflected") {
      auto meta = MetaDataOf<const IncompleteType* const* const&>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "const IncompleteType* const* const");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == false);
      REQUIRE(meta->mIsNullifiable == false);
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         REQUIRE(meta->mPoolTactic == PoolTactic::Default);
      #endif
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage == ::std::max(Alignment, Roof2(sizeof(IncompleteType*))));
      REQUIRE(meta->mIsAbstract == false);
      REQUIRE(meta->mSize == sizeof(void*));
      REQUIRE(meta->mAlignment == alignof(void*));
      REQUIRE(meta->mDeptr == MetaDataOf<const IncompleteType* const>());
      REQUIRE(meta->mOrigin == nullptr);
      REQUIRE(meta->mIsConstant == true);
      REQUIRE(meta->mDeptr->mIsConstant == true);
      REQUIRE(meta->mDecvq == MetaDataOf<IncompleteType**>());
      REQUIRE(meta->mDeptr->mDecvq == MetaDataOf<IncompleteType*>());

      REQUIRE(meta->mBases.size() == 0);
      REQUIRE(meta->mAbilities.size() == 0);
      REQUIRE(meta->mMembers.size() == 0);
      REQUIRE(meta->mNamedValues.size() == 0);
      REQUIRE(meta->mConvertersTo.size() == 0);
      REQUIRE(meta->mConvertersFrom.size() == 0);
   }

   WHEN("const IncompleteType* const* const&& reflected") {
      auto meta = MetaDataOf<const IncompleteType* const* const&&>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "const IncompleteType* const* const");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == false);
      REQUIRE(meta->mIsNullifiable == false);
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         REQUIRE(meta->mPoolTactic == PoolTactic::Default);
      #endif
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage == ::std::max(Alignment, Roof2(sizeof(IncompleteType*))));
      REQUIRE(meta->mIsAbstract == false);
      REQUIRE(meta->mSize == sizeof(void*));
      REQUIRE(meta->mAlignment == alignof(void*));
      REQUIRE(meta->mDeptr == MetaDataOf<const IncompleteType* const>());
      REQUIRE(meta->mOrigin == nullptr);
      REQUIRE(meta->mIsConstant == true);
      REQUIRE(meta->mDeptr->mIsConstant == true);
      REQUIRE(meta->mDecvq == MetaDataOf<IncompleteType**>());
      REQUIRE(meta->mDeptr->mDecvq == MetaDataOf<IncompleteType*>());

      REQUIRE(meta->mBases.size() == 0);
      REQUIRE(meta->mAbilities.size() == 0);
      REQUIRE(meta->mMembers.size() == 0);
      REQUIRE(meta->mNamedValues.size() == 0);
      REQUIRE(meta->mConvertersTo.size() == 0);
      REQUIRE(meta->mConvertersFrom.size() == 0);
   }
}

SCENARIO("A complex type reflected with CTTI traits", "[metadata]") {
   WHEN("ImplicitlyReflectedDataWithTraits reflected") {
      ImplicitlyReflectedDataWithTraits instance;
      auto ptrtobase = static_cast<ImplicitlyReflectedData*>(&instance);
      const auto baseoffset = reinterpret_cast<char*>(ptrtobase) - reinterpret_cast<char*>(&instance);
      auto meta = MetaDataOf<ImplicitlyReflectedDataWithTraits>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "MyType");
      REQUIRE(meta->mInfo == "Info about MyType");
      REQUIRE(meta->mFileExtensions == "txt, pdf");
      REQUIRE(meta->mVersionMajor == 2);
      REQUIRE(meta->mVersionMinor == 1);
      REQUIRE(meta->mIsDeep == true);
      REQUIRE(meta->mIsPOD == false); // not POD due to being abstract
      REQUIRE(meta->mIsNullifiable == false); // not nullifiable due to being abstract
      IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta->mPoolTactic == PoolTactic::Size));
      REQUIRE(meta->mConcreteRetriever()->Is<ImplicitlyReflectedData>());
      REQUIRE(meta->mAllocationPage == Roof2(250 * sizeof(ImplicitlyReflectedDataWithTraits)));
      REQUIRE(meta->mIsAbstract == true);
      REQUIRE(meta->mSize == sizeof(ImplicitlyReflectedDataWithTraits));
      REQUIRE(meta->mAlignment == alignof(ImplicitlyReflectedDataWithTraits));
      REQUIRE(meta->mOrigin == meta);
      REQUIRE(meta->mIsConstant == false);
      REQUIRE(meta->mDeptr == nullptr);
      REQUIRE(meta->mDecvq == MetaDataOf<ImplicitlyReflectedDataWithTraits>());

      REQUIRE(meta->mBases.size() == 1);
      REQUIRE(meta->mBases[0].mType->Is<ConvertibleData>());
      REQUIRE(meta->mBases[0].mImposed == false);
      REQUIRE(meta->mBases[0].mBinaryCompatible == false);
      REQUIRE(meta->mBases[0].mCount == 1);
      REQUIRE(baseoffset >= 0);
      REQUIRE(meta->mBases[0].mOffset == static_cast<Offset>(baseoffset));

      REQUIRE(meta->mAbilities.size() == 1);
      REQUIRE(meta->mAbilities.begin()->first->Is<Verbs::Create>());
      REQUIRE(meta->mAbilities.begin()->second.mVerb->Is<Verbs::Create>());
      REQUIRE(meta->mAbilities.begin()->second.mOverloadsConstant.size() == 1);
      REQUIRE(meta->mAbilities.begin()->second.mOverloadsConstant.contains(Ability::Signature {}));
      REQUIRE(meta->mAbilities.begin()->second.mOverloadsMutable.size() == 1);
      REQUIRE(meta->mAbilities.begin()->second.mOverloadsMutable.contains(Ability::Signature {}));

      REQUIRE(meta->mMembers.size() == 3);
      /*REQUIRE(meta->mMembers[0].mCount == 1);
      REQUIRE(meta->mMembers[0].mValueRetriever(&instance) == &instance.member);
      REQUIRE(meta->mMembers[0].GetTrait(0) == nullptr);
      REQUIRE(meta->mMembers[0].GetType()->Is<int>());*/

      REQUIRE(meta->mMembers[0].mCount == 1);
      REQUIRE(meta->mMembers[0].mValueRetriever(&instance) == &instance.anotherMember);
      REQUIRE(meta->mMembers[0].GetTrait(0)->Is<Traits::Tag>());
      REQUIRE(meta->mMembers[0].GetTrait(1) == nullptr);
      REQUIRE(meta->mMembers[0].GetType()->Is<bool>());

      REQUIRE(meta->mMembers[1].mCount == 12);
      REQUIRE(meta->mMembers[1].mValueRetriever(&instance) == instance.anotherMemberArray);
      REQUIRE(meta->mMembers[1].GetTrait(0) == nullptr);
      REQUIRE(meta->mMembers[1].GetType()->Is<int>());

      REQUIRE(meta->mMembers[2].mCount == 1);
      REQUIRE(meta->mMembers[2].mValueRetriever(&instance) == &instance.sparseMember);
      REQUIRE(meta->mMembers[2].GetTrait(0) == nullptr);
      REQUIRE(meta->mMembers[2].GetType()->Is<int>());

      REQUIRE(meta->mNamedValues.size() == 0);

      const auto intmeta = MetaDataOf<int>();
      REQUIRE(meta->mConvertersTo.size() == 1);
      REQUIRE(meta->mConvertersTo.at(intmeta).mType->Is<int>());
      REQUIRE(meta->mConvertersTo.at(intmeta).mFunction);
      REQUIRE(meta->GetConverter(intmeta) == meta->mConvertersTo.at(intmeta).mFunction);

      const auto pimeta = MetaDataOf<Pi>();
      REQUIRE(meta->mConvertersFrom.size() == 1);
      REQUIRE(meta->mConvertersFrom.at(pimeta).mType->Is<Pi>());
      REQUIRE(meta->mConvertersFrom.at(pimeta).mFunction);
      REQUIRE(meta->GetConverter(pimeta) == nullptr);

      REQUIRE(pimeta->GetConverter(meta) == meta->mConvertersFrom.at(pimeta).mFunction);

      int converted = 1;
      meta->GetConverter(intmeta)(&instance, &converted);
      REQUIRE(converted == 664);

      Pi source;
      ImplicitlyReflectedDataWithTraits convertedFromPi1;
      pimeta->GetConverter(meta)(&source, &convertedFromPi1);
      REQUIRE(convertedFromPi1.member == 314);
   }

   WHEN("CheckingWhatGetsInherited reflected") {
      CheckingWhatGetsInherited instance;
      ImplicitlyReflectedData* ptrtobase = &static_cast<ImplicitlyReflectedData&>(instance);
      const auto baseoffset = reinterpret_cast<char*>(ptrtobase) - reinterpret_cast<char*>(&instance);
      auto meta = MetaDataOf<CheckingWhatGetsInherited>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mInfo == "Info about MyType");
      REQUIRE(meta->mFileExtensions == "txt, pdf");
      REQUIRE(meta->mVersionMajor == 2);
      REQUIRE(meta->mVersionMinor == 1);
      REQUIRE(meta->mIsDeep == true);
      REQUIRE(meta->mIsPOD == false);           // not POD due to being abstract
      REQUIRE(meta->mIsNullifiable == false);   // not nullifiable due to being abstract
      IF_LANGULUS_MANAGED_MEMORY(REQUIRE(meta->mPoolTactic == PoolTactic::Size));
      REQUIRE(meta->mConcreteRetriever()->Is<ImplicitlyReflectedData>());
      REQUIRE(meta->mAllocationPage == Roof2(250 * sizeof(ImplicitlyReflectedDataWithTraits)));
      REQUIRE(meta->mIsAbstract == true);
      REQUIRE(meta->mSize == sizeof(ImplicitlyReflectedDataWithTraits));
      REQUIRE(meta->mAlignment == alignof(ImplicitlyReflectedDataWithTraits));
      REQUIRE(meta->mOrigin == meta);
      REQUIRE(meta->mIsConstant == false);
      REQUIRE(meta->mDeptr == nullptr);
      REQUIRE(meta->mDecvq == MetaDataOf<CheckingWhatGetsInherited>());

      REQUIRE(meta->mBases.size() == 1);
      REQUIRE(meta->mBases[0].mType->Is<ConvertibleData>());
      REQUIRE(meta->mBases[0].mImposed == false);
      REQUIRE(meta->mBases[0].mBinaryCompatible == false);
      REQUIRE(meta->mBases[0].mCount == 1);
      REQUIRE(baseoffset >= 0);
      REQUIRE(meta->mBases[0].mOffset == static_cast<Offset>(baseoffset));

      REQUIRE(meta->mAbilities.size() == 1);
      REQUIRE(meta->mAbilities.begin()->first->Is<Verbs::Create>());
      REQUIRE(meta->mAbilities.begin()->second.mVerb->Is<Verbs::Create>());
      REQUIRE(meta->mAbilities.begin()->second.mOverloadsConstant.size() == 1);
      REQUIRE(meta->mAbilities.begin()->second.mOverloadsConstant.contains(Ability::Signature {}));
      REQUIRE(meta->mAbilities.begin()->second.mOverloadsMutable.size() == 1);
      REQUIRE(meta->mAbilities.begin()->second.mOverloadsMutable.contains(Ability::Signature {}));

      REQUIRE(meta->mMembers.size() == 0);
      REQUIRE(meta->mNamedValues.size() == 0);

      const auto intmeta = MetaDataOf<int>();
      REQUIRE(meta->mConvertersTo.size() == 1);
      REQUIRE(meta->mConvertersTo.at(intmeta).mType->Is<int>());
      REQUIRE(meta->mConvertersTo.at(intmeta).mFunction);
      REQUIRE(meta->GetConverter(intmeta) == meta->mConvertersTo.at(intmeta).mFunction);

      const auto pimeta = MetaDataOf<Pi>();
      REQUIRE(meta->mConvertersFrom.size() == 1);
      REQUIRE(meta->mConvertersFrom.at(pimeta).mType->Is<Pi>());
      REQUIRE(meta->mConvertersFrom.at(pimeta).mFunction);
      REQUIRE(meta->GetConverter(pimeta) == nullptr);

      REQUIRE(pimeta->GetConverter(meta) == meta->mConvertersFrom.at(pimeta).mFunction);

      int converted = 1;
      meta->GetConverter(intmeta)(&instance, &converted);
      REQUIRE(converted == 664);

      Pi source;
      CheckingWhatGetsInherited convertedFromPi1;
      pimeta->GetConverter(meta)(&source, &convertedFromPi1);
      REQUIRE(convertedFromPi1.member == 314);
   }
}

SCENARIO("A simple type reflected with CTTI traits", "[metadata]") {
   WHEN("ImplicitlyReflectedData reflected") {
      auto meta = MetaDataOf<ImplicitlyReflectedData>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "ImplicitlyReflectedData");
      REQUIRE(meta->mCppName == "ImplicitlyReflectedData");
      REQUIRE(meta->mInfo == "<no info provided>");
      REQUIRE(meta->mFileExtensions == "ASE");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == true);
      REQUIRE(meta->mIsNullifiable == false);
      #if LANGULUS_FEATURE(MANAGED_MEMORY)
         REQUIRE(meta->mPoolTactic == PoolTactic::Default);
      #endif
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage >= Alignment);
      REQUIRE(meta->mIsAbstract == false);
      REQUIRE(meta->mSize == sizeof(ImplicitlyReflectedData));
      REQUIRE(meta->mAlignment == alignof(ImplicitlyReflectedData));

      REQUIRE(meta->mNamedValues.size() == 3);
      REQUIRE(meta->mConvertersTo.size() == 0);
      REQUIRE(meta->mConvertersFrom.size() == 0);
   }

   WHEN("ForcedAbstract reflected") {
      auto meta = MetaDataOf<ForcedAbstract>();

      static_assert(not CT::HasIntentAssign<Langulus::Moved, PureVirtual>);
      static_assert(::std::assignable_from<PureVirtual&, PureVirtual&&>);

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "ForcedAbstract");
      REQUIRE(meta->mCppName == "ForcedAbstract");
      REQUIRE(meta->mInfo == "<no info provided>");
      REQUIRE(meta->mFileExtensions == "");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == false);
      REQUIRE(meta->mIsNullifiable == false);
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage >= Alignment);
      REQUIRE(meta->mIsAbstract == true);
      REQUIRE(meta->mSize == sizeof(ForcedAbstract));
      REQUIRE(meta->mAlignment == alignof(ForcedAbstract));
      REQUIRE(meta->mNamedValues.empty());
      REQUIRE(meta->mConvertersTo.empty());
      REQUIRE(meta->mConvertersFrom.empty());
   }

   WHEN("PureVirtual reflected") {
      auto meta = MetaDataOf<PureVirtual>();

      static_assert(not CT::HasIntentAssign<Langulus::Moved, PureVirtual>);
      static_assert(::std::assignable_from<PureVirtual&, PureVirtual&&>);

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "PureVirtual");
      REQUIRE(meta->mCppName == "PureVirtual");
      REQUIRE(meta->mInfo == "<no info provided>");
      REQUIRE(meta->mFileExtensions == "");
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
      REQUIRE(meta->mIsDeep == false);
      REQUIRE(meta->mIsPOD == false);
      REQUIRE(meta->mIsNullifiable == false);
      REQUIRE(meta->mConcreteRetriever == nullptr);
      REQUIRE(meta->mAllocationPage >= Alignment);
      REQUIRE(meta->mIsAbstract == true);
      REQUIRE(meta->mSize == sizeof(PureVirtual));
      REQUIRE(meta->mAlignment == alignof(PureVirtual));
      REQUIRE(meta->mNamedValues.empty());
      REQUIRE(meta->mConvertersTo.empty());
      REQUIRE(meta->mConvertersFrom.empty());
   }
}

SCENARIO("A reflected verb with CTTI traits", "[metaverb]") {
   GIVEN("Create verb with positive/negative tokens, with stateless and contextual default functions") {
      WHEN("Reflected") {
         Anyness::Many someBlock;
         Flow::Verb someVerb;
         auto meta = MetaVerbOf<Verbs::Create>();

         REQUIRE(meta != nullptr);
         REQUIRE(meta->mToken == "Create");
         REQUIRE(meta->mTokenReverse == "Destroy");
         REQUIRE(meta->mInfo.starts_with("Used for allocating new elements. "));
         REQUIRE(meta->mVersionMajor == 1);
         REQUIRE(meta->mVersionMinor == 0);
         REQUIRE(meta->mOperator == " + ");
         REQUIRE(meta->mPrecedence == 5);
         REQUIRE(meta->mOperatorReverse == " - ");
         REQUIRE(meta->mDefaultInvocationMutable);
         REQUIRE(meta->mDefaultInvocationMutable(someBlock, someVerb) == false);
         REQUIRE(meta->mDefaultInvocationConstant);
         REQUIRE(meta->mDefaultInvocationConstant(someBlock, someVerb) == true);
         REQUIRE(meta->mStatelessInvocation);
         REQUIRE(meta->mStatelessInvocation(someVerb) == false);
      }
   }
}

void FunctionForTesting(void*) {
   Logger::Verbose("Executed FunctionForTesting");
}

SCENARIO("A reflected function signature", "[function]") {
   GIVEN("A reflected function pointer") {
      using Signature = void(*)(void*);

      static_assert(    ::std::is_function_v<Deptr<Signature>>);
      static_assert(    CT::Sparse<Signature>);
      static_assert(not CT::Decayed<Signature>);
      static_assert(    CT::Complete<Signature>);

      auto meta = MetaDataOf<Signature>();

      REQUIRE(meta != nullptr);
      REQUIRE(meta->mToken == "Function<void(void*)>*");
      REQUIRE(meta->mIsSparse);
      REQUIRE(meta->mVersionMajor == 1);
      REQUIRE(meta->mVersionMinor == 0);
   }

   /*GIVEN("A reflected function reference (shouldn't compile)") {
      auto FuncRef = FunctionForTesting;
      using Signature = decltype(*FuncRef);

      WHEN("Reflected") {
         static_assert(    ::std::is_function_v<Deref<Signature>>);
         static_assert(    CT::Dense<Signature>);
         static_assert(not CT::Decayed<Signature>);
         static_assert(not CT::Complete<Signature>);

         auto meta = MetaData::Of<Signature>();
         REQUIRE(meta != nullptr);

         THEN("Requirements should be met") {
            REQUIRE(meta->mToken == "void(*)(void*)");
            REQUIRE(meta->mIsSparse);
            REQUIRE(meta->mVersionMajor == 1);
            REQUIRE(meta->mVersionMinor == 0);
         }
      }
   }*/
}
